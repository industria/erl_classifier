%%%-------------------------------------------------------------------
%%% File    : ec_term_manager.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Term manager used for building the term vocabulary.
%%%
%%% Created : 23 Apr 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_term_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: update(Term | [Term]) -> {ok, TermId} | {ok, [TermId]}
%% Description: Update the term vocabulary a single term or with 
%% a list of terms. The function will check if any of the terms exists
%% already and returns the existing id before creating a new one.
%% Note: This is the single point that will write to the vocabulary.
%%--------------------------------------------------------------------
update(Term) when is_binary(Term) ->
    gen_server:call(?SERVER, {update, Term});
update(Terms) when is_list(Terms) ->
    gen_server:call(?SERVER, {update_list, Terms}).
	    


%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
handle_call({update, Term}, _From, State) ->
    Reply = term_lookup_update(Term),
    {reply, Reply, State};

handle_call({update_list, Terms}, _From, State) ->
    TermUpdate = fun(Term, Acc) ->
			 {ok, TermId} = term_lookup_update(Term),
			 [TermId | Acc]
		 end,
    TermIds = lists:foldl(TermUpdate, [], Terms),
    %% Reverse the list so it has the same as the terms list
    Reply = {ok, lists:reverse(TermIds)},
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

term_lookup_update(Term) ->
    case ec_store:term_id(Term) of
	{ok, TermId} ->
	    {ok, TermId};
	unknown ->
	    ec_store:new_term(Term)
    end.
