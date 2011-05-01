%%%-------------------------------------------------------------------
%%% File    : ec_classifier.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Classifier - classify documents
%%% These servers are started dynamically from the launcher supervisor.
%%%
%%% Created :  5 Apr 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_classifier).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/1, classify/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% Function: classify(Document) -> [ {atom(), measure} ] 
%% Description: Get a list of classes and measure for the document.
%%--------------------------------------------------------------------
classify(Pid, Class, Document) ->
    ReplyTo = self(), %% The calling process (a ec_any_of process)
    gen_server:cast(Pid, {classify_log, Class, Document, ReplyTo}).

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
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({classify_log, Class, Document, ReplyTo}, State) ->
    %% Timing instrumentation
    Inst_Start = now(),

    %% Probability of class and ^class occuring is 
    %% estimated by class document occurence p(c) = nc / n
    {ok, DocsMatch, DocsComp} = ec_store:doc_freq(Class),
    DocsTotal = DocsMatch + DocsComp,
    %% P(c)  : Probability of class (Match)
    Pc = DocsMatch / DocsTotal,
    %% P(^c) : Probability of ^class (Complement)
    PcC = DocsComp / DocsTotal,
    %% Total number of term occurences for class and ^class
    {ok, Tctm, TctCm} = ec_store:vocabulary_size(Class),
    %% Vocabulary size B
    {ok, B} = ec_store:vocabulary_size(),
    %% Function for calculating 
    %% P(t|c)  : Propability of term given class (Match)
    %% P(t|^c) : Probability of term given ^class (Complement)
    %% For each term in the document
    F = fun({TermId, TermCount}, {AccPtc, AccPtcC}) ->
		%% Term occurence in training documents for class and ^class
		{ok, Tct, TctC} = ec_store:term_freq(Class, TermId),
		%% Documents are frequency distributions so log(P(t|c))
		%% needs to be multiplied by the term count.
		%% 1 is added to compencate for training data sparseness.
		%% B is vocabulary size
		Ptc = math:log((Tct + 1) / (Tctm + B)) * TermCount,
		PtcC = math:log((TctC + 1) / (TctCm + B)) * TermCount,
		{AccPtc + Ptc, AccPtcC + PtcC}
	end,
    {SumPtc, SumPtcC} = lists:foldl(F, {0, 0}, Document),
    %% P(c|d) : Probability of class given document (Match)
    Pcd = math:log(Pc) + SumPtc,    
    %% P(^c|d) : Probalility of ^class given document (Complement)
    PcdC = math:log(PcC) + SumPtcC,

    %% Calculate time use and post event
    Inst_MicroSeconds = timer:now_diff(now(), Inst_Start),
    ec_event_classifier:stat_time(Class, Inst_MicroSeconds),

    ec_any_of:result(ReplyTo, Class, Pcd, PcdC),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

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


