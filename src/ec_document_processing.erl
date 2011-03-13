%%%-------------------------------------------------------------------
%%% File    : ec_document_processing.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Document processing functions, support library.
%%%
%%% Created : 10 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_document_processing).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([remove_punctuation/1, normalize_whitespace/1, to_lowercase/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: remove_punctuation(Document) -> Document
%% Description: Remove the punctuation from the input Document
%% Punctuation is defined as: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
%%--------------------------------------------------------------------
remove_punctuation(Document) when is_binary(Document) ->
    << <<X>> || <<X>> <= Document, not punctuation(<<X>>) >>.

%%--------------------------------------------------------------------
%% Function: normalize_whitespace(Document) -> Document
%% Description: Changes whitespace control characters to a space
%%--------------------------------------------------------------------
normalize_whitespace(Document) when is_binary(Document) ->
    << <<(normalize_whitespace_character(X))>> || <<X>> <= Document >>.

%%--------------------------------------------------------------------
%% Function: to_lowercase(Document) -> Document
%% Description: Changes the document to lowercase.
%%--------------------------------------------------------------------
to_lowercase(Document) when is_binary(Document) ->    
    << <<(string:to_lower(X))/utf8>> || <<X/utf8>> <= Document >>.
%%====================================================================
%% Internal functions
%%====================================================================
punctuation(X) when is_binary(X)->
    %% Punctuation defined as  !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    ((<<16#21>> =< X andalso X =< <<16#2F>>) orelse
     (<<16#3A>> =< X andalso X =< <<16#40>>) orelse
     (<<16#5B>> =< X andalso X =< <<16#60>>) orelse
     (<<16#7B>> =< X andalso X =< <<16#7E>>)).

normalize_whitespace_character(X) when (16#09 =< X andalso X =< 16#0D) ->
    16#20;
normalize_whitespace_character(X) ->
    X.

is_whitespace_control(X) ->    
    %% Whitespace control characters not including space
    (<<16#09>> =< X andalso X =< <<16#0D>>).

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

remove_punctuation_test() ->
    Doc = <<"Remove, the. punctuation"/utf8>>,
    Expected = <<"Remove the punctuation"/utf8>>,
    ?assertEqual(Expected, remove_punctuation(Doc)).

normalize_whitespace_test() ->
    Document = <<"fish \tcat\n"/utf8>>,
    Normalized = normalize_whitespace(Document),
    ?assertEqual(<<"fish  cat "/utf8>>, Normalized).

punctuation_test() ->
    ?assert(punctuation(<<"!"/utf8>>)),
    ?assertNot(punctuation(<<"a"/utf8>>)).

is_whitespace_control_test() ->
    ?assert(is_whitespace_control(<<"\t"/utf8>>)),
    ?assertNot(is_whitespace_control(<<" "/utf8>>)),
    ?assertNot(is_whitespace_control(<<"g"/utf8>>)).

to_lowercase_test() ->
    ?assertEqual(<<"\x{E6}bler"/utf8>>, to_lowercase(<<"\x{C6}bler"/utf8>>)).

-endif.
