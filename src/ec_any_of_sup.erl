%%%-------------------------------------------------------------------
%%% File    : ec_any_of_sup.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Supervisor for launching any-of classifiers,
%%% which makes the initial document processing and coordinates
%%% the required N processes launched throufh the supervisor
%%% ec_classifier_launcher_sup (ec_classifier processes).
%%%
%%% Created : 23 Apr 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_any_of_sup).

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
    Classes = ec_configuration:classes(),
    Language = ec_configuration:language(),
    MinTermLen = ec_configuration:min_term_length(),
    MaxTermLen = ec_configuration:max_term_length(),
    Args = [ Classes, Language, MinTermLen, MaxTermLen ],
    CoordServer = {ec_any_of, 
		      {ec_any_of, start_link, Args},
		      temporary, 
		      brutal_kill, 
		      worker, 
		      [ec_any_of]},

    {ok,{{simple_one_for_one, 0, 1}, [CoordServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
