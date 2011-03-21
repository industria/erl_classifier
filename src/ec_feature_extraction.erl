%%%-------------------------------------------------------------------
%%% File    : ec_feature_extraction.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implements feature extractions.
%%%
%%% Created : 13 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_feature_extraction).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([features/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: features(Language, Document) -> Frequency distribution
%% Description: Extract features from a Document returning them
%% in the form of a frequency distribution.
%% Language is an atom representing the language, 
%% only danish is supported for this version.
%%--------------------------------------------------------------------
features(danish, Document) when is_binary(Document) ->
    DP = ec_document_processing:remove_punctuation(Document),
    DN = ec_document_processing:normalize_whitespace(DP),
    Low = ec_document_processing:to_lowercase(DN),
    Terms = ec_tokenizer:word_tokenize(Low),
    StemmedTerms = [ ec_danish_stemmer:stem(Term) || Term <- Terms],
    ec_frequency_distribution:create(StemmedTerms).

%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Testing
%%====================================================================

-ifdef(TEST).

features_swedish_test() ->
    ?assertError(function_clause, features(swedish, << >>)).

features_danish_test() ->
    {ok, Document} = file:read_file("test/documents/polsport.txt"),
    FD = features(danish, Document),
    ?assertMatch({_, 2}, lists:keyfind(<<"bold"/utf8>>, 1, FD)),
    ?assertMatch({_, 3}, lists:keyfind(<<"kik"/utf8>>, 1, FD)),
    ?assertMatch({_, 2}, lists:keyfind(<<"selvtillid"/utf8>>, 1, FD)),
    ?assertMatch({_, 2}, lists:keyfind(<<"fejlaflevering"/utf8>>, 1, FD)).

-endif.
