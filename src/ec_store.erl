%%%-------------------------------------------------------------------
%%% File    : ec_store.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Handles storage for the classifier.
%%%
%%% Created : 30 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_store).

%% API
-export([init_tables/0, delete_tables/0, add_document/2, term/1, 
	 term_frequency/1, term_class_frequency/2, update_term_class_frequency/3,
	 classes/0, ndocuments/0, ndocuments_in_class/1]).

%% Final exports are collected below in one export of each area
-export([stopword_update/0, is_stopword/2]).
-export([term_id/1, new_term/1]).

-record(ids, {table, id}).

-record(terms, {term, term_id}).

-record(term_class_frequency, {class_term_id, count}).

-record(document_class_frequency, {class, count}).

-record(term_frequency, {term_id, count}).

-record(stopwords, {language_term, dummy = unused}).
%% Mnesia tables must have at least one extra attribute in addition
%% to the key, hence the dummy attribute in the record definition.

%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% Function: is_stopword(Language, Term) -> boolean
%% Description: Check for the {Language, Term} key indicating 
%% that the term is defined as a stopword for the Language.
%% Note: Applications should use the API in ec_stopwords this module
%% should be considered internal.
%%--------------------------------------------------------------------
is_stopword(Language, Term) ->
    Key =  {Language, Term},
    case mnesia:dirty_read(stopwords, Key) of
	[_] ->
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: stopword_update()
%% Description: Update the stopwords table with the stopwords found
%% in stopword directory.
%%--------------------------------------------------------------------
stopword_update() ->
    %% [ { lang, [words] } ]
    StopwordLists = ec_stopwords:stopword_lists(),
    lists:foreach(
      fun({Language, Terms}) ->
	      lists:foreach(
		fun(Term) ->
			R = #stopwords{language_term={Language, Term}},
			mnesia:dirty_write(stopwords, R)
		end, Terms)
      end, StopwordLists),
    ok.


%%--------------------------------------------------------------------
%% Function: term_id(Term) -> {ok, TermId} | unknown
%% Description: Lookup the id of a term.
%%--------------------------------------------------------------------
term_id(Term) when is_binary(Term) ->
    case mnesia:dirty_read({terms, Term}) of
	[Terms] -> 
	    {ok, Terms#terms.term_id};
	_ -> 
	    unknown
    end.

%%--------------------------------------------------------------------
%% Function: new_term(Term) -> {ok, TermId}
%% Description: Add a new term to the vocabulary.
%%--------------------------------------------------------------------
new_term(Term) when is_binary(Term) ->
    TermId = mnesia:dirty_update_counter({ids, term_id}, 1),
    TermRecord = #terms{term = Term, term_id = TermId},
    mnesia:dirty_write(TermRecord),
    {ok, TermId}.

    


%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
init_tables() ->
    create_tables().

delete_tables() ->
    mnesia:delete_table(ids),
    mnesia:delete_table(terms),
    mnesia:delete_table(term_frequency),
    mnesia:delete_table(term_class_frequency),
    mnesia:delete_table(document_class_frequency),
    mnesia:delete_table(stopwords).


%%--------------------------------------------------------------------
%% Function: classes() -> [ class = atom() ]
%% Description: Get a list of classes known to the classifier.
%%--------------------------------------------------------------------
classes() ->
    T = fun() ->
		mnesia:foldl(fun(Record, NewAcc) ->
				     [Record#document_class_frequency.class | NewAcc]
					 end, [], document_class_frequency)
		    end,
    {atomic, Classes} = mnesia:transaction(T),
    Classes.

ndocuments() ->
    T = fun() ->
		mnesia:foldl(fun(Record, NewAcc) ->
				     NewAcc + Record#document_class_frequency.count
			     end, 0, document_class_frequency)
	end,
    {atomic, N} = mnesia:transaction(T),
    N.

ndocuments_in_class(Class) ->
    case mnesia:dirty_read({document_class_frequency, Class}) of
	[C] ->
	    C#document_class_frequency.count;
	_ ->
	    0
    end.
    



%%--------------------------------------------------------------------
%% Function: add_document(Class, DocumentFrequency)
%% Description: Add a document represented as a document frequency
%%              list to a given class.
%%--------------------------------------------------------------------
add_document(Class, FrequencyDistribution) ->
    T = fun() -> 
		update_document_class_frequency(Class),
		Updater = fun({Term, Count}) ->
				  TermId = term(Term),
				  update_term_frequency(TermId, Count),
				  update_term_class_frequency(TermId, Class, Count)
			  end,
		lists:foreach(Updater, FrequencyDistribution)
    end,
    mnesia:transaction(T).


%%--------------------------------------------------------------------
%% Function: term(Term) -> TermId 
%% Description: Return id for the Term, 
%%              the Term is added ti terms table if it doesn't exist
%%--------------------------------------------------------------------
term(Term) ->
    case mnesia:dirty_read({terms, Term}) of
	[T] ->
	    T#terms.term_id;
	_ ->
	    %% Create a new Term entry
	    Trans = fun() ->
		TermId = mnesia:dirty_update_counter({ids, term_id}, 1),
		TermEntry = #terms{term = Term, term_id = TermId},
		mnesia:write(TermEntry),
		TermId
	    end,
	    {atomic, Id} = mnesia:transaction(Trans),
	    Id
    end.


    
term_frequency(TermId) when is_integer(TermId) ->
    case mnesia:dirty_read({term_frequency, TermId}) of
	[TF] -> 
	    TF#term_frequency.count;
	 _ -> 
	    0
    end.


%%--------------------------------------------------------------------
%% Function: term_class_frequency(Term, Class) -> Integer()
%% Description: Get the term class frequency for a class (binary term).
%%--------------------------------------------------------------------
term_class_frequency(Term, Class) when is_binary(Term), is_atom(Class) ->
    case mnesia:dirty_read({terms, Term}) of
	[Terms] -> term_class_frequency(Terms#terms.term_id, Class);
	_ -> 0
    end;

%%--------------------------------------------------------------------
%% Function: term_class_frequency(TermId, Class) -> Integer()
%% Description: Get the term class frequency for a class
%%--------------------------------------------------------------------
term_class_frequency(TermId, Class) when is_integer(TermId), is_atom(Class) ->
    Class_TermId = {Class, TermId},
    case mnesia:dirty_read({term_class_frequency, Class_TermId}) of
	[Freq] -> Freq#term_class_frequency.count;
	_ -> 0
    end.


%%--------------------------------------------------------------------
%% Function: update_term_class_frequency(TermId, Class, Count)
%% Description: Updates the class termid with count,
%%              creating a new entry if one doesn't exists.
%%--------------------------------------------------------------------
update_term_class_frequency(TermId, Class, Count) ->
    Class_TermId = {Class, TermId},
    mnesia:transaction(fun() ->
	case mnesia:wread({term_class_frequency, Class_TermId}) of		       
	    [CT] ->
		%% Update the record
		NewCount = Count + CT#term_class_frequency.count,
		CTC = CT#term_class_frequency{count = NewCount},
		mnesia:write(CTC);
	     _ ->
		%% New record
		NewCT = #term_class_frequency{class_term_id = Class_TermId, 
					      count = Count},
		mnesia:write(NewCT)
	end
    end).
%%====================================================================
%% Internal functions
%%====================================================================
create_tables() ->
    mnesia:create_table(ids, 
    			[{type, set},
    			 {disc_copies, [node()]},
    			 {attributes, record_info(fields, ids)}
    			]),
    mnesia:create_table(terms, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, terms)}
			]),
    mnesia:create_table(term_frequency, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, term_frequency)}
			]),
    mnesia:create_table(term_class_frequency, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, term_class_frequency)}
			]),
    mnesia:create_table(document_class_frequency, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, document_class_frequency)}
			]),
    mnesia:create_table(stopwords, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stopwords)}
			]),

    mnesia:wait_for_tables([terms, term_class_frequency], 60000).



update_document_class_frequency(Class) ->
    case mnesia:wread({document_class_frequency, Class}) of
	[DCT] ->
	    Count = DCT#document_class_frequency.count + 1,
	    Updated = DCT#document_class_frequency{count = Count},
	    mnesia:write(Updated);
	_ -> 
	   New = #document_class_frequency{class = Class, count = 1},
	   mnesia:write(New)
    end.
		      
		      
	    
update_term_frequency(TermId, Count) ->
    case mnesia:wread({term_frequency, TermId}) of
	[TF] ->
	    NewCount = Count + TF#term_frequency.count,
	    Updated = TF#term_frequency{count = NewCount},
	    mnesia:write(Updated);
	_ ->
	    New = #term_frequency{term_id = TermId, count = Count},
	    mnesia:write(New)
    end.



 
    
