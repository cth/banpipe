:- object(list_extras).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/06,
		comment is 'Various semi-useful predicates for working with lists.']).
		
	:- public(intersperse/3).
	:- info(intersperse/3,
		[ comment is 'Interspersed is is List with Separator is inserted between every two elements of List',
		  argnames is ['Separator','List','Interspersed']]).
	intersperse(_,[],[]).
	intersperse(_,[One],[One]).
	intersperse(Separator,[One,Two|Rest],[One,Separator|NewRest]) :-
	        intersperse(Separator,[Two|Rest],NewRest).
	
	:- public(sublist_split/3).
	:- info(sublist_split/3,
		[ comment is 'Splits List into SubList occuring between Separator elements of List',
		  argnames is ['Sep','List','SubLists']]).
	sublist_split(_,[],[]).
	sublist_split(Sep,List,[SubList1|RestSublists]) :-
		list::append(SubList1,[Sep|RestList],List),
		!,
		sublist_split(Sep,RestList,RestSublists).
	sublist_split(_,List,[List]).
:- end_object.
