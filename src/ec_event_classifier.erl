%%%-------------------------------------------------------------------
%%% File    : ec_event_classifier.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Event stream API for the classifiers.
%%%
%%% Implementing a event handler (gen_event behaviour) you have
%%% to be able to handle the following messages in handle_event:
%%%
%%% {time, MicroSeconds} : Amount of time used by a single two-class.
%%%
%%% Created : 25 Apr 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_event_classifier).

%% API
-export([start_link/0, add_handler/2, delete_handler/2]).

-export([stat_time/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link 
%% Description: Wrapper for gen_event:start_link so the application
%% doesn't have to know the name globally but can just used this API
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% Function: add_handler(Handler, Args) 
%% Description: Wrapper for gen_event:add_handler so the application
%% doesn't have to know the name globally but can just used this API
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% Function: delete_handler(Handler, Args)
%% Description: Wrapper for gen_event:delete_handler so the application
%% doesn't have to know the name globally but can just used this API
%%--------------------------------------------------------------------
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).


%%--------------------------------------------------------------------
%% Function: stat_time(MicroSeconds)
%% Description: The amount of time in MicroSeconds used by a two-class
%% classifier to calculate the P(c|d) and P(^c|d).
%% Message to handle : {time, MicroSeconds}
%%--------------------------------------------------------------------
stat_time(MicroSeconds) ->
    gen_event:notify(?SERVER, {time, MicroSeconds}).



%%====================================================================
%% Internal functions
%%====================================================================
