%%%-------------------------------------------------------------------
%%% File    : ec_coach.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : The coach coordinates the training of the two-class
%%% classifiers making up the complete any-of classifier.
%%%
%%% Created : 23 Apr 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_coach).

-behaviour(gen_server).

%% API
-export([start_link/1, train/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {classtrainers, 
		language,
	        min_term_length,
		max_term_length
	       }).

%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% Function: train(Class, Document) -> ok.
%% Description: Adds a document to the classifier.
%% This actually means adding it to the number of positiv/negativ 
%% classifiers that make up the overall classifier. 
%% If an unknown class is given the document will be classified as
%% negativ match on all the defined classifiers.
%%--------------------------------------------------------------------
train(Class, Document) when is_atom(Class), is_binary(Document) ->
    gen_server:call(?SERVER, {train, [Class], Document});
train(Classes, Document) when is_list(Classes), is_binary(Document) ->
    gen_server:call(?SERVER, {train, Classes, Document});
train(Class, Filename) when is_atom(Class), is_list(Filename) ->
    read_file_and_train(Filename, [Class], fun train/2);
train(Classes, Filename) when is_list(Classes), is_list(Filename) ->
    read_file_and_train(Filename, Classes, fun train/2).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ClassTrainers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ClassTrainers, []).

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
init(ClassTrainers) ->
    {ok, #state{classtrainers = ClassTrainers, 
		language = ec_configuration:language(),
		min_term_length = ec_configuration:min_term_length(),
		max_term_length = ec_configuration:max_term_length()
	       }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({train, Classes, Document}, _From, State) ->
    %% 1) Normalize from whitespace, punctuation and case
    NormalizedDocument = ec_document_processing:normalize(Document),

    %% 2) Get the document tokenized
    Terms = ec_tokenizer:word_tokenize(NormalizedDocument),

    %% 3) Remove term outside of the length interval
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
    {ok, TermIds} = ec_term_manager:update(Stemmed),

    %% 7) Create a frequency distribution
    TFD = ec_frequency_distribution:create(TermIds),

    %% 8) Push the term lists to the class trainers
    lists:foreach(fun(Trainer) ->
			  ec_class_trainer:update_with_document(Trainer, Classes, TFD)
		  end, State#state.classtrainers),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
%% Func: read_file_and_train(Filename, TrainingFunction)
%% Description: Read the file to a binary and call the TrainingFunction
%% with the Classes list. This is to avoiding repeating the
%% read_file in case for each file based train function.
%%--------------------------------------------------------------------
read_file_and_train(Filename, Classes, TrainingFunction) ->
    case file:read_file(Filename) of
	{ok, Document} ->
	    TrainingFunction(Classes, Document);
	{error, Reason} ->
	    {error, Reason}
    end.
