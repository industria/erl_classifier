%%%-------------------------------------------------------------------
%%% File    : ec_docfile.erl
%%% Author  : James Lindstorff <james@ind-w700ds>
%%% Description : Implement functions for loading a document file
%%% into the classifier. The document file is structured with one
%%% document per line and the classes comma separared at the end of
%%% the line. This means that the document should be cleared for
%%% commas before inserted into a document file. A line example:
%%% documenttext,class1,class2
%%%
%%% Created : 29 May 2011 by James Lindstorff <james@ind-w700ds>
%%%-------------------------------------------------------------------
-module(ec_docfile).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([train/1, classes/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: train(Docfile) 
%% Description: Train the classifier with a document file.
%%--------------------------------------------------------------------
train(Docfile) ->
    F = fun(Classes, Document) ->
		io:fwrite("~p ~n", [Classes])
	end,
    train(Docfile, F, void).

%%--------------------------------------------------------------------
%% Function: classes(Docfile) -> [atom()]
%% Description: Return a list of class atoms present in the Docfile.
%%--------------------------------------------------------------------
classes(Docfile) ->
    ClassDict = dict:new(),
    Trainer = fun(Classes, _Document, Acc) ->
		      lists:foldl(fun(Class, AccInner) ->
					  dict:update_counter(Class, 1, AccInner)
				  end, Acc, Classes)
	      end,
    R = train(Docfile, Trainer, ClassDict),
    lists:sort(dict:fetch_keys(R)).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: train(Docfile, Trainer, Acc) -> Acc 
%% Description: Train the classifier with a document file using the
%% trainer fun, Trainer(Class, Document, Acc) -> Acc
%%--------------------------------------------------------------------
train(Docfile, Trainer, Acc) ->
    Modes = [read, binary, read_ahead, {encoding, utf8}],
    {ok, IoDevice} = file:open(Docfile, Modes),
    read_docfile_line(IoDevice, Trainer, Acc).

%%--------------------------------------------------------------------
%% Function: read_docfile_line(IoDevice, Trainer, Acc) 
%% Description: Read a line from the IoDevice and call Trainer
%% fun with Classes and Document extracted from line. 
%% This function closes the IoDevice on eof.
%%--------------------------------------------------------------------
read_docfile_line(IoDevice, Trainer, Acc) ->
    case io:get_line(IoDevice, "") of
	eof ->
	    file:close(IoDevice),
	    Acc;
	Line ->
	    {ok, Document, Classes} = line_processor(Line),
	    NewAcc = Trainer(Classes, Document, Acc),
	    read_docfile_line(IoDevice, Trainer, NewAcc)
    end.

%%--------------------------------------------------------------------
%% Function: line_processor(Line) -> {ok, Document, [classes atoms]}
%% Description: Expects a binary line in docfile format.
%%--------------------------------------------------------------------
line_processor(Line) ->
    Tokens = binary:split(Line, <<",">>, [global]),
    [Document | Classes] = Tokens,
    ClassAtoms = [ filtered_bin_to_atom(C) || C <- Classes],
    {ok, Document, ClassAtoms}.

%%--------------------------------------------------------------------
%% Function: filtered_bin_to_atom(Class) -> atom()
%% Description: Given a binary class filter out any LF.
%% This is basically an effect of the way we read the file using
%% io:get_line which includes a LF at the end. The reason for pushing
%% the filtering all the way down to this level is that we don't 
%% have to copy the longer line.
%%--------------------------------------------------------------------
filtered_bin_to_atom(Class) ->
    FilteredClass = << <<C>> || <<C>> <= Class, C =/= 10 >>,
    binary_to_atom(FilteredClass, utf8).

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

filtered_bin_to_atom_test() ->
    ?assertEqual(class1, filtered_bin_to_atom(<<"class1\n">>)),
    ?assertEqual(class1, filtered_bin_to_atom(<<"class1">>)).

line_processor_test() ->
    Line = <<"denne del er dokumentet,class1,class2\n"/utf8>>,
    {ok, Document, Classes} = line_processor(Line),
    ?assertEqual(<<"denne del er dokumentet"/utf8>>, Document),
    ?assertEqual([class1, class2], Classes).

train_test() ->
    Trainer = fun(Classes, Document, Acc) ->
		      ?assert(0 < length(Classes)),
		      ?assert(0 <  bit_size(Document)),
		      Acc
	      end,
    train("test/docfile/docfile_test", Trainer, void).

classes_test() ->
    R = classes("test/docfile/docfile_test"),
    ?assertEqual([indland,kultur,musik,udland], R).

-endif.
