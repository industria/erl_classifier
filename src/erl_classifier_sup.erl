%%%-------------------------------------------------------------------
%%% File    : erl_classifier_sup.erl
%%% Author  : James Lindstorff <james@ind-x301>
%%% Description : Root supervisor for Naive Bayes classifier.
%%%
%%% Created : 15 Mar 2011 by James Lindstorff <james@ind-x301>
%%%-------------------------------------------------------------------
-module(erl_classifier_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    TrainerServer = {ec_trainer,{ec_trainer, start_link, []},
		     permanent, 2000, worker, [ec_trainer]},
    ClassifyServer = {ec_classifier, {ec_classifier, start_link, []},
		      permanent, 2000, worker, [ec_classifier]},
    Children = [TrainerServer, ClassifyServer],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
