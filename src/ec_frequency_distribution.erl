%%%-------------------------------------------------------------------
%%% File    : ec_frequency_distribution.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Frequency distribution
%%%
%%% Created : 10 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_frequency_distribution).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([create/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: create
%% Description: Create a frequency distribution from a term list.
%% Return: List of {term, count} tuples.
%%--------------------------------------------------------------------
create(TermList) when is_list(TermList)->
    FD = lists:foldl(
      fun(Element, AccIn) -> 
	      dict:update_counter(Element, 1, AccIn) 
      end, dict:new(), TermList),
    dict:to_list(FD).

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

create_non_list_test() ->
    ?assertError(function_clause, create(10)).

create_binary_list_test() ->
    Terms = [<<"Fish"/utf8>>, <<"Fish"/utf8>>, <<"Horse"/utf8>>],
    Expected = [{<<"Fish"/utf8>>, 2}, {<<"Horse"/utf8>>, 1}],
    FD = create(Terms),
    %% Sort both frequency distribution and expected terms
    %% because the dict doesn't have order.
    ExpectedSorted = lists:keysort(1, Expected),
    FDS = lists:keysort(1, FD),
    ?assertEqual(ExpectedSorted, FDS).

-endif.
