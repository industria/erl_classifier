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
-export([doc_freq_update/2]).
-export([term_freq_update/3]).
-export([doc_freq/1]).
-export([term_freq/2]).
-record(ids, {table, id}).

-record(terms, {term, term_id}).

-record(term_class_frequency, {class_term_id, count}). %% Will be obsolete

-record(document_class_frequency, {class, count}). %% Will be obsolete

%% Contain document counts for a class
%% match_count + complement_count is the total number of documents
%% for a class
-record(doc_class_freq, {class, match_count, complement_count}).

-record(term_frequency, {term_id, count}). %% Will be obsolete


-record(term_class_freq, {term_class, match_count, complement_count}).

%% Mnesia tables must have at least one extra attribute in addition
%% to the key, hence the dummy attribute in the record definition.
-record(stopwords, {language_term, dummy = unused}).

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
%% Function: doc_freq_update(Class, Match) -> ok
%% Description: Update the document frequency for a given class.
%% Match is a boolean indicating class match or class complement.
%%--------------------------------------------------------------------
doc_freq_update(Class, Match) ->
    R = case mnesia:dirty_read(doc_class_freq, Class) of
	    [DCF] ->
		update_doc_freq(DCF, Match);
	    _ ->
		new_doc_freq(Class, Match)
    end,
    mnesia:dirty_write(doc_class_freq, R).


new_doc_freq(Class, true) ->
    #doc_class_freq{class = Class, match_count = 1, complement_count = 0};
new_doc_freq(Class, false) ->
    #doc_class_freq{class = Class, match_count = 0, complement_count = 1}.

update_doc_freq(DF, true) ->
    C = DF#doc_class_freq.match_count + 1,
    DF#doc_class_freq{match_count = C};
update_doc_freq(DF, false) ->
    C = DF#doc_class_freq.complement_count + 1,
    DF#doc_class_freq{complement_count = C}.


%%--------------------------------------------------------------------
%% Function: Term_freq_update(Class, Match, FreqDist) -> ok
%% Description: Update the term frequency for a given class.
%% Match is a boolean indicating class match or class complement.
%%--------------------------------------------------------------------
term_freq_update(Class, Match, FreqDist) ->
    lists:foreach(
      fun({TermId, Count}) ->
	      Key = {Class, TermId},
	      R = case mnesia:dirty_read(term_class_freq, Key) of
		      [TCF] ->
			  update_term_freq(TCF, Count, Match);
		      _ ->
			  new_term_freq(Key, Count, Match)
		  end,
	      mnesia:dirty_write(term_class_freq, R)
      end, FreqDist),
    ok.

new_term_freq(ClassTerm, Count, true) ->
    #term_class_freq{term_class = ClassTerm, 
		     match_count = Count,
		     complement_count = 0};
new_term_freq(ClassTerm, Count, false) ->
    #term_class_freq{term_class = ClassTerm,
		     match_count = 0,
		     complement_count = Count}.

update_term_freq(TF, Count, true) ->    
    C = TF#term_class_freq.match_count + Count,
    TF#term_class_freq{match_count = C};
update_term_freq(TF, Count, false) ->
    C = TF#term_class_freq.complement_count + Count,
    TF#term_class_freq{complement_count = C}.


%%--------------------------------------------------------------------
%% Function: doc_freq(Class) -> {ok, Match, Complement} 
%% Description: Get the document frequency for a class.
%%--------------------------------------------------------------------
doc_freq(Class) ->
    case mnesia:dirty_read(doc_class_freq, Class) of
	[Freq] ->
	    {ok, Freq#doc_class_freq.match_count, Freq#doc_class_freq.complement_count};
	_ ->
	    {ok, 0, 0}
    end.


%%--------------------------------------------------------------------
%% Function: term_freq(Class, TermId) -> {ok, Match, Complement} 
%% Description: Get the term frequency for a class.
%%--------------------------------------------------------------------
term_freq(Class, TermId) ->
%%-record(term_class_freq, {term_class, match_count, complement_count}).
    case mnesia:dirty_read(term_class_freq, {Class, TermId}) of
	[Freq] ->
	    {ok, Freq#term_class_freq.match_count, Freq#term_class_freq.complement_count};
	_ ->
	    {ok, 0, 0}
    end.


%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
init_tables() ->
    create_tables().

delete_tables() ->
    mnesia:delete_table(ids),
    mnesia:delete_table(terms),
    mnesia:delete_table(term_frequency),  %% Will be obsolete
    mnesia:delete_table(term_class_frequency),  %% Will be obsolete
    mnesia:delete_table(document_class_frequency), %% Will be obsolete
    mnesia:delete_table(doc_class_freq),
    mnesia:delete_table(term_class_freq),
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
%% Will be obsolete
    mnesia:create_table(term_frequency, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, term_frequency)}
			]),
%% Will be obsolete
    mnesia:create_table(term_class_frequency, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, term_class_frequency)}
			]),
%% Will be obsolete
    mnesia:create_table(document_class_frequency, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, document_class_frequency)}
			]),

    mnesia:create_table(doc_class_freq, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, doc_class_freq)}
			]),

    mnesia:create_table(term_class_freq, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, term_class_freq)}
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



 
    
