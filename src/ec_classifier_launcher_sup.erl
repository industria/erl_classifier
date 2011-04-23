%%%-------------------------------------------------------------------
%%% File    : ec_classifier_launcher_sup.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Supervisor for launcing classification processes
%%% based on simple one-on-one model.
%%%
%%% Created : 23 Apr 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_classifier_launcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    ClassifyServer = {ec_classifier, 
		      {ec_classifier, start_link, []},
		      temporary, 
		      brutal_kill, 
		      worker, 
		      [ec_classifier]},

    {ok,{{simple_one_for_one, 0, 1}, [ClassifyServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
