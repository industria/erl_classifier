%%%-------------------------------------------------------------------
%%% File    : ec_stopwords.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Handling stopword lists
%%%
%%% Created : 22 Apr 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_stopwords).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API
-export([is_stopword/2, stopword_lists/0]).

-define(SUFFIX, "stopwords").
-define(STOPWORD_LOCATION, "priv/stopwords").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: is_stopword(Language, Term) -> boolean
%% Description: Will check is the Term is a stopword in the given
%% Language. Unknown languages are defined as having no stopwords.
%%--------------------------------------------------------------------
is_stopword(Language, Term) when is_atom(Language), is_binary(Term) ->
    ec_store:is_stopword(Language, Term).

%%--------------------------------------------------------------------
%% Function: stopword_lists() -> [ { lang, [words] } ]
%% Description: Return the stopword lists defined as a list of
%% tuples containing the language as an atom and a list of bitstrings
%% representing the stopword terms.
%%--------------------------------------------------------------------
stopword_lists() ->
    stopword_lists(?STOPWORD_LOCATION).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: stopword_lists(Location) -> [ { lang, [words] } ]
%% Description: See stopword_lists/0 in the API section.
%%--------------------------------------------------------------------
stopword_lists(Location) ->
    StopwordFiles = stopword_files(Location),
    lists:foldl(fun(StopwordFile, StopwordLists) ->
			Language = language_from_filename(StopwordFile),
			File = filename:join([Location, StopwordFile]),
			Words = read_stopword_file(File),
			[{Language, Words} | StopwordLists]
		end, [], StopwordFiles).

%%--------------------------------------------------------------------
%% Function: read_stopword_file(Filename) -> [Binary]
%% Description: Reads a stopword file returning the content as
%% a list of stopword terms represented as binaries.
%%--------------------------------------------------------------------
read_stopword_file(Filename) ->
    Modes = [read, binary, read_ahead, {encoding, utf8}],
    {ok, IoDevice} = file:open(Filename, Modes),
    read_stopword_file_line(IoDevice, []).

%%--------------------------------------------------------------------
%% Function: read_stopword_file_file(IoDevice, Words)
%% Description: Read a line from the IoDevice removing any trailing
%% new line (\n), excluding empty lines and returning a list of terms.
%%--------------------------------------------------------------------
read_stopword_file_line(IoDevice, Words) ->
    case io:get_line(IoDevice, "") of
	eof ->
	    file:close(IoDevice),
	    Words;
	Line ->
	    %% Remove the optional \n at the end of the line
	    FilteredLine = << <<X>> || <<X>> <= Line, X =/= 10 >>,
	    if 
		bit_size(FilteredLine) =:= 0 ->
		    read_stopword_file_line(IoDevice, Words);
		true ->
		    read_stopword_file_line(IoDevice, [FilteredLine | Words])
	    end
    end.

%%--------------------------------------------------------------------
%% Function: stopword_files(Location) -> [Filenames]
%% Description: Retrieves a list of stopword files from Location.
%%--------------------------------------------------------------------
stopword_files(Location) ->
    {ok, Filenames} = file:list_dir(Location),
    %% Files are named lang.stopwords so all .stopwords are included. 
    [ X || X <- Filenames,  is_stopword_file(X)].

%%--------------------------------------------------------------------
%% Function: is_stopword_file(Filename) -> true|false
%% Description: Check if filename as the stopword suffix
%%--------------------------------------------------------------------
is_stopword_file(Filename) ->
    Elements = string:tokens(Filename, "."),
    Suffix = lists:last(Elements),
    (Suffix =:= ?SUFFIX).

%%--------------------------------------------------------------------
%% Function: language_from_filename(Filename) -> atom()
%% Description: Stop words files are expected to be named on the form
%% language.stopwords and this function will return everything before
%% the last . as an atom representing the language.
%%--------------------------------------------------------------------
language_from_filename(Filename) ->
    Elements = string:tokens(Filename, "."),
    Language = hd(Elements),
    list_to_atom(Language).

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).


expected_stopwords() ->
     [<<"var"/utf8>>, <<"vi"/utf8>>, <<"vil"/utf8>>, <<"ville"/utf8>>, <<"vor"/utf8>>].

is_stopword_file_test() ->
    ?assertNot(is_stopword_file("fisk")),
    ?assertNot(is_stopword_file("hest.stopword")),
    ?assert(is_stopword_file("danish.stopwords")).

stopword_files_test() ->
    Files = stopword_files("test/stopwords"),
    SortedFiles = lists:sort(Files),
    ExpectedFileList = ["danish.stopwords", "demo.stopwords", "swedish.stopwords"],
    ?assertEqual(ExpectedFileList, SortedFiles).

read_stopword_file_test() ->
    ExpectedWords = expected_stopwords(),
    Words = read_stopword_file("test/stopwords/demo.stopwords"),
    %% Words are in file-reverse order so we sort for the test
    SortedWords = lists:sort(Words),
    ?assertEqual(ExpectedWords, SortedWords).

language_from_filename_test() ->
    DemoLang = language_from_filename("demo.stopwords"),
    ?assertEqual(demo, DemoLang),
    DoubleDot = language_from_filename("demo.another.stopwords"),
    ?assertEqual(demo, DoubleDot).


stopword_lists_test() ->
    SL = stopword_lists("test/stopwords"),
    Languages = [ L || {L, _} <- SL],
    SortedLanguages = lists:sort(Languages),
    ExpectedLanguages = [danish, demo, swedish],
    ?assertEqual(ExpectedLanguages, SortedLanguages),
    {_, DL} = lists:keyfind(demo, 1, SL),
    SortedDL = lists:sort(DL),
    ExpectedWords = expected_stopwords(),
    ?assertEqual(ExpectedWords, SortedDL).

-endif.
