%%%-------------------------------------------------------------------
%%% File    : ec_configuration.erl
%%% Author  : James Lindstorff <james@ind-x301>
%%% Description : Easy and encapsulated access to the configuration
%%% supplied in the env element of the app file.
%%%
%%% Created :  7 May 2011 by James Lindstorff <james@ind-x301>
%%%-------------------------------------------------------------------
-module(ec_configuration).

%% API
-export([classes/0, language/0]).

-define(APPLICATION, erl_classifier).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: classes() -> [Class]
%% Description: Return the classes defined in the application env.
%%--------------------------------------------------------------------
classes() ->
    {ok, Classes} = application:get_env(?APPLICATION, classes),
    Classes.


language() ->
    {ok, Language} = application:get_env(?APPLICATION, language),
    Language.

%%====================================================================
%% Internal functions
%%====================================================================
