%%%-------------------------------------------------------------------
%%% File    : ec_any_of.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Any-of coordinating process.
%%% It does initial document processing and coordinates the classification
%%% using processes launched through ec_classifier_launcher_sup.
%%%
%%% Classification works by the following protocol:
%%% 1) An any-of process (this module) is lunched and a blocking call
%%%    is made to classify a document.
%%% 2) This process, does initial document processing, so the document
%%%    is represented by a term id frequency distribution.
%%% 3) After initial processing, this process starts N classifiers, 
%%%    N being the number of two-class classifiers defined during
%%%    startup and calls each of them asynchronously.
%%% 4) After the classifiers has been launched a class to classifier
%%%    Pid map is stored in state. The reply send is a noreply 
%%%    with a 5 second timeout to allow for a deferred reply, when the
%%%    classifiers just launched has finished their job. The caller 
%%%    remains blocked for 5 seconds on the gen_server:call.
%%% 5) While the caller is blocked the classifiers will start
%%%    calling the result function, which will register the result
%%%    in state and remove the Pid, that send the result, from
%%%    the class to classifier Pid map generated earlier. After the
%%%    result function the classifier called in this module will
%%%    send a stop message to the classifier process, effectivly 
%%%    terminating it after job done.
%%% 6) Classification of a document is finished, when the result 
%%%    function have been called for all the classifiers started,
%%%    which is triggered by the class to classifier Pid map being
%%%    empty, at which point the actual reply, which was previously 
%%%    deferred, will be performed with the result from state being
%%%    the reply.
%%%
%%% Created : 23 Apr 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_any_of).

-behaviour(gen_server).

%% API
-export([start_link/4, classify/1, classify_detail/1, result/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {classes, 
		language,
		min_term_length,
		max_term_length,
		pidmap, 
		results = [], 
		deferredreplyclient}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Classes, Language, MinTermLen, MaxTermLen) ->
    gen_server:start_link(?MODULE, [ Classes, Language, MinTermLen, MaxTermLen ], []).


%%--------------------------------------------------------------------
%% Function: classify(Filename) -> [Class] | {error, Reason}
%% Description: Return a list of classes matching the document read
%% from the file pointed to by Filename.
%% {error, Reason} is the error from file:read_file passed through.
%%--------------------------------------------------------------------
classify(Filename) when is_list(Filename) ->
    read_file_and_classify(Filename, fun classify/1);
%%--------------------------------------------------------------------
%% Function: classify(Document) -> [Class]
%% Description: Return a list of classes matching the document.
%%--------------------------------------------------------------------
classify(Document) when is_binary(Document)->
    {ok, Result} = classify_detail(Document),
    [ Class || {Class, Match, Complement} <- Result, (Match > Complement)].


%%--------------------------------------------------------------------
%% Function: classify_detail(Filename) -> {ok, [{class, match, complement}]} |
%%                                         fail_timeout |
%%                                         {error, Reason}
%% Description: Return classification detail for a file. See the
%% classify_detail for binary document for details on whats returned.
%% {error, Reason} is the error from file:read_file passed through.
%%--------------------------------------------------------------------
classify_detail(Filename) when is_list(Filename) ->
    read_file_and_classify(Filename, fun classify_detail/1);
%%--------------------------------------------------------------------
%% Function: classify_detail(Document) -> {ok, [{class, match, complement}]} |
%%                                        fail_timeout
%% Description: Return a list of tuples tagged with the class and
%% a match and a complement proof. The proofs are the logrithmic sums
%% from the two-class classifier, where the largest proof indicates 
%% the class the document belongs to. This function should
%% only be called if the numbers are of interest, otherwise calling
%% classify/1 will give a list of classes matching the document 
%% without all the details.
%%--------------------------------------------------------------------
classify_detail(Document) when is_binary(Document) ->
    {ok, Pid} = ec_any_of_sup:start_child(),
    R = try gen_server:call(Pid, {classify, Document}) of
	Result ->
		{ok, Result}
	catch
	    exit:{timeout, _} ->
		%% TODO: if any classifying processes is still
		%% running they should be stopped
		fail_timeout
	end,

    %% Terminate the any_of process
    gen_server:cast(Pid, stop),
    R.

%%--------------------------------------------------------------------
%% Function: result(Pid, Class, Match, Complement)
%% Description: Callback API-function used by a two-class classifiers
%% to signal that is has classified a document. This API-function
%% sends the any-of coordinator a result message registering the result.
%% After registering the result with the any-of coordinater 
%% the two-class classifier asks itself to stop.
%%--------------------------------------------------------------------
result(Pid, Class, Match, Complement) ->
    ResultMessage = {result, Class, Match, Complement},
    {ok, PidThatReplied} = gen_server:call(Pid, ResultMessage),
    ec_classifier:stop(PidThatReplied).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ Classes, Language, MinTermLen, MaxTermLen ]) ->
    {ok, #state{classes = Classes, 
		language = Language,
		min_term_length = MinTermLen,
		max_term_length = MaxTermLen
	       }
    }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({classify, Document}, From, State) ->
    %% 1) Normalize from whitespace, punctuation and case
    NormalizedDocument = ec_document_processing:normalize(Document),

    %% 2) Get the document tokenized
    Terms = ec_tokenizer:word_tokenize(NormalizedDocument),

    %% 3) Remove term outside of the length interval
    %%    term_length_in_range(Term, Min, Max)
    %% 4) Remove stopwords from the list 
    %% 5) Stem the words
    Language = State#state.language,
    Min = State#state.min_term_length,
    Max = State#state.max_term_length,
    Stemmed = [ ec_stemming:stem(Language, T) 
		|| T <- Terms, 
		   ec_document_processing:term_length_in_range(T, Min, Max) 
		   andalso (not ec_stopwords:is_stopword(Language, T))],

    %% 6) Get the tokens converted into term ids
    TermIds = terms_to_ids(Stemmed),

    %% 7) Create a frequency distribution
    TFD = ec_frequency_distribution:create(TermIds),

    %% Generate the N two-class classifiers
    Launch = fun(Class, Acc) ->
		     {ok, Pid} = ec_classifier_launcher_sup:start_child(),
		     ec_classifier:classify(Pid, Class, TFD),
		     dict:store(Pid, Class, Acc)
	     end,
    PidMap = lists:foldl(Launch, dict:new(), State#state.classes),
    %% Register the pid map and the client for the deferred reply in the state

    NewState = State#state{pidmap = PidMap,
			   deferredreplyclient = From},
    %% Deferred reply when the results are in we use gen_server:reply
    %% waiting a maximum of 5 seconds for the replies
    {noreply, NewState, 5000};

handle_call({result, Class, Match, Complement}, From, State) ->
    %% Map the caller (From) to the PidMap
    {FromPid, _FromTag} = From,
    PidMap = State#state.pidmap,
    {ok, _Class} = dict:find(FromPid, PidMap),

    %% Remove the pid mapping from the state (a zero length mapping means all have answered)
    NewPidMap = dict:erase(FromPid, PidMap),

    %% Add the recieved result to the process state
    Results = State#state.results,
    NewState = State#state{results = [{Class, Match, Complement} | Results],
			   pidmap = NewPidMap},


    %% Check if all processes has answered
    case dict:size(NewPidMap) =:= 0 of
	true ->
	    Client = NewState#state.deferredreplyclient,
	    ReplyResults = NewState#state.results,
	    gen_server:reply(Client, ReplyResults),
	    ok;
	false ->
	    ok
    end,
 

    %% Return the Pid from the state PidMap
    Reply = {ok, FromPid},

    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
        {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: terms_to_ids(Terms) -> [TermId | unknown]
%% Description: Convert terms list into a list of term ids.
%%--------------------------------------------------------------------
terms_to_ids(Terms) ->
    lists:foldl(fun(Term, Ids) -> 
			case ec_store:term_id(Term) of
			    {ok, Id} ->
				[Id | Ids];
			    unknown ->
				[unknown | Ids]
			end
		end, [], Terms).

%%--------------------------------------------------------------------
%% Func: read_file_and_classify(Filename, ClassificationFunction)
%% Description: Helper function for classify/1 and classify_detail/1
%% in their file versions. It takes the filename reads it and applies
%% the classification function given. Used to avoid repeating the
%% case construct.
%%--------------------------------------------------------------------
read_file_and_classify(Filename, ClassificationFunction) ->
    case file:read_file(Filename) of
	{ok, Document} ->
	    ClassificationFunction(Document);
	{error, Reason} ->
	    {error, Reason}
    end.
