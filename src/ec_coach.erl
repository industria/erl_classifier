%%%-------------------------------------------------------------------
%%% File    : ec_coach.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Coach coordinating the training of all the 
%%% positiv / negativ classifiers making up the complete classifier.
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

-record(state, {classtrainers}).

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
    gen_server:call(?SERVER, {train, Classes, Document}).


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
    {ok, #state{classtrainers=ClassTrainers}}.

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

    %% 3) Remove stopwords from the list 
    %% 4) Stem the words
    Stemmed = [ ec_danish_stemmer:stem(T) || T <- Terms, not ec_stopwords:is_stopword(danish, T)],

    %% 5) Get the tokens converted into term ids
    {ok, TermIds} = ec_term_manager:update(Stemmed),

    %% 6) Create a frequency distribution
    TFD = ec_frequency_distribution:create(TermIds),

    %% 7) Push the term lists to the class trainers
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
