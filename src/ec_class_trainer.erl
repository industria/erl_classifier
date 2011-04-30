%%%-------------------------------------------------------------------
%%% File    : ec_class_trainer.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Implements class trainer.
%%%
%%% Created : 22 Apr 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_class_trainer).

-behaviour(gen_server).

%% API
-export([start_link/2, update_with_document/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {class}).

%%====================================================================
%% API
%%====================================================================

update_with_document(SpecId, Classes, Document) ->
    gen_server:cast(SpecId, {update_with_document, Classes, Document}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SpecId, Class) ->
    gen_server:start_link({local, SpecId}, ?MODULE, Class, []).

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
init(Class) ->
    State = #state{class=Class},
    {ok, State}.

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
handle_cast({update_with_document, Classes, Document}, State) ->
    %% 1) Is this document a positiv match for the classifier
    Match = lists:any(
	      fun(Class) ->
		      Class =:= State#state.class
	      end, Classes),
    %% 2) Update document class count
    ec_store:doc_freq_update(State#state.class, Match),
    %% 3) Update class term counts
    ec_store:term_freq_update(State#state.class, Match, Document),
    %% 4) Update class vocabulary counts
    TermsInDocument = lists:foldl(fun({_TermId, Count}, Sum) ->
					  Sum + Count
				  end, 0, Document),
    ec_store:update_class_vocabulary(State#state.class, TermsInDocument, Match),
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
