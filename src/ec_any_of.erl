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
-export([start_link/1, classify/1, classify_detail/1, result/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {classes, pidmap, results = [], deferredreplyclient}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Classes) ->
    gen_server:start_link(?MODULE, [ Classes ], []).


classify(Document) ->
    {ok, _Result, Classes} = classify_detail(Document),
    Classes.

classify_detail(Document) ->
    {ok, Pid} = ec_any_of_sup:start_child(),
    R = try gen_server:call(Pid, {classify, Document}) of
	Result ->
		%% This contains the result of the classify call
		Classes = [ Class || 
			      {Class, Match, Complement} <- Result,
			      (Match > Complement) 
			  ],
		{ok, Result, Classes}
	catch
	    exit:{timeout, _} ->
		%% TODO: if any classifying processes is still
		%% running they should be stopped
		fail_timeout
	end,

    %% Terminate the any_of process
    gen_server:cast(Pid, stop),
    R.

result(Pid, Class, Match, Complement) ->
    {ok, PidThatReplied} = gen_server:call(Pid, {result, Class, Match, Complement}),
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
init([ Classes ]) ->
    {ok, #state{classes = Classes}}.

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

    %% 3) Remove stopwords from the list 
    %% 4) Stem the words
    Stemmed = [ ec_danish_stemmer:stem(T) || T <- Terms, not ec_stopwords:is_stopword(danish, T)],

    %% 5) Get the tokens converted into term ids
    TermIds = terms_to_ids(Stemmed),

    %% 6) Create a frequency distribution
    TFD = ec_frequency_distribution:create(TermIds),

    %% Generate the N class classifiers
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

terms_to_ids(Terms) ->
    lists:foldl(fun term_to_id/2, [], Terms).

term_to_id(Term, Ids) ->
    case ec_store:term_id(Term) of
	{ok, Id} ->
	    [Id | Ids];
	unknown ->
	    [unknown | Ids]
    end.

