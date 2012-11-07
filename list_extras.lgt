
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
:- end_object.
