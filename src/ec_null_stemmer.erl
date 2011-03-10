%%%-------------------------------------------------------------------
%%% File    : ec_null_stemmer.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implements a null stemmer.
%%% 
%%%
%%% Created : 10 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_null_stemmer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([stem/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: stem
%% Description: Pass through stemmer for use when no stemming is
%% needed in the processing chain.
%%--------------------------------------------------------------------
stem(Word) ->
    Word.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

stem_test() ->
    ?assertEqual("fishtank", stem("fishtank")).

-endif.
