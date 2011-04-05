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
	 term_class_frequency/2, update_term_class_frequency/3]).

-record(ids, {table, id}).

-record(terms, {term, term_id}).

-record(term_class_frequency, {class_term_id, count}).

-record(document_class_frequency, {class, count}).

-record(term_frequency, {term_id, count}).

%%====================================================================
%% API
%%====================================================================
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
    mnesia:delete_table(document_class_frequency).


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
				  update_term_class_frequency(Term, Class, Count)
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
%% Function: update_term_class_frequency(Term, Class, Count)
%% Description: Updates the class term with count,
%%              creating a new entry if one doesn't exists.
%%--------------------------------------------------------------------
update_term_class_frequency(Term, Class, Count) when is_binary(Term), 
						     is_atom(Class), 
						     is_integer(Count) ->
    TermId = term(Term),
    update_term_class_frequency(TermId, Class, Count);

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
	    NewCount = TF#term_frequency.count + 1,
	    Updated = TF#term_frequency{count = NewCount},
	    mnesia:write(Updated);
	_ ->
	    New = #term_frequency{term_id = TermId, count = Count},
	    mnesia:write(New)
    end.
