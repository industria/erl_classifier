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
-export([remove_punctuation/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: remove_punctuation
%% Description: Remove the punctuation from the input Document
%% Punctuation is defined as: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
%%--------------------------------------------------------------------
remove_punctuation(Document) when is_binary(Document) ->
    << <<X>> || <<X>> <= Document, include_character(<<X>>) >>.
    
%%====================================================================
%% Internal functions
%%====================================================================
include_character(X) ->
    not (is_punctuation(X) orelse is_whitespace_control(X)).

is_punctuation(X) ->
    %% Punctuation defined as  !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    ((<<16#21>> =< X andalso X =< <<16#2F>>) orelse
     (<<16#3A>> =< X andalso X =< <<16#40>>) orelse
     (<<16#5B>> =< X andalso X =< <<16#60>>) orelse
     (<<16#7B>> =< X andalso X =< <<16#7E>>)).

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

include_character_test() ->
    ?assert(include_character(<< "\x{C6}"/utf8 >>)),
    ?assert(include_character(<< "\X{20}"/utf8 >>)),
    ?assertNot(include_character(<< "\x{0D}"/utf8 >>)).


is_punctuation_test() ->
    ?assert(is_punctuation(<<"!"/utf8>>)),
    ?assertNot(is_punctuation(<<"a"/utf8>>)).

is_whitespace_control_test() ->
    ?assert(is_whitespace_control(<<"\t"/utf8>>)),
    ?assertNot(is_whitespace_control(<<" "/utf8>>)),
    ?assertNot(is_whitespace_control(<<"g"/utf8>>)).

-endif.
