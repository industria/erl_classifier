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
-export([classes/0, language/0, min_term_length/0, max_term_length/0]).

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

min_term_length() ->
    {ok, Length} = application:get_env(?APPLICATION, min_term_length),
    Length.

max_term_length() ->
    {ok, Length} = application:get_env(?APPLICATION, max_term_length),
    Length.
    
%%====================================================================
%% Internal functions
%%====================================================================
