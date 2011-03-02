%%%-------------------------------------------------------------------
%%% File    : ec_danish_stemmer_tests.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Tests for the ec_danish_stemmer
%%%
%%% Created :  2 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_danish_stemmer_tests).

-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
init_test() ->
    {ok, R1, R2} = ec_danish_stemmer:stem("Beautiful"),
    ?assertEqual("iful", R1),
    ?assertEqual("ul", R2).

