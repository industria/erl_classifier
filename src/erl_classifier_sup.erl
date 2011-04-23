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
-export([start_link/1]).

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
start_link(Classes) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Classes).

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
init(Classes) ->
    %% Child specification is a tuple like:
    %% {ID, Start, Restart, Shutdown, Type, Modules}
    %% ID : identified the specification internally
    %% Start :  {M, F, A} top start 
    %% Restart: Restart strategy: permanent, temporary or transient
    %% Shutdown: Integer milliseconds for soft, brutal_kill or infinity
    %% Type: Process type, supervisor or worker
    %% Modules: The modules the process depends on (for hot code upgrades)

    TrainingSup = {ec_training_sup,
		   {ec_training_sup, start_link, [ Classes ]},
		   permanent,
		   infinity,
		   supervisor,
		   [ec_training_sup]},

    AnyOfLauncher = {ec_any_of_sup,
		     {ec_any_of_sup, start_link, [ Classes ]},
		     permanent,
		     infinity,
		     supervisor,
		     [ec_any_of_sup]},

    ClassifyLauncher = {ec_classifier_launcher_sup,
			{ec_classifier_launcher_sup, start_link, []},
			permanent,
			infinity,
			supervisor,
			[ec_classifier_launcher_sup]},

    Children = [TrainingSup, AnyOfLauncher, ClassifyLauncher],
    %% Restart: {How, Max, Within}
    %% production rule of thumb is 4 in 3600 = four per hour
    RestartStrategy = {one_for_one, 0, 1},
    {ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
