%%%-------------------------------------------------------------------
%%% File    : ec_training_sup.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Supervisor for the training processes. 
%%% There is a training process for each class in the classifier.
%%% For a simple positiv/negativ classifier only one process is needed.
%%%
%%% Created : 22 Apr 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_training_sup).

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
    supervisor:start_link({local, ?SERVER}, ?MODULE, [f1, cykling, fodbold]).

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
    CB = fun(Class, Children) ->
		 ClassString = atom_to_list(Class),
		 SpecString = string:join(["ec_ct_", ClassString], ""),
		 SpecId = list_to_atom(SpecString),
		 MFA = {ec_class_trainer, start_link, [SpecId, Class]},
		 Child = {SpecId,
			  MFA, 
			  permanent,
			  2000,
			  worker,
			  [ec_class_trainer]
			 },
		 [Child | Children]
    end,
    ClassTrainerChildren = lists:foldl(CB, [], Classes),

    TermManagerChild = {ec_term_manager, 
			{ec_term_manager, start_link, []},
			permanent,
			2000,
			worker,
			[ec_term_manager]
		       },
    ChildrenToStart = [TermManagerChild | ClassTrainerChildren],
    %% TODO: Remember to update the restart strategy 4, 3600
    {ok,{{one_for_all,0,1}, ChildrenToStart}}.

%%====================================================================
%% Internal functions
%%====================================================================
