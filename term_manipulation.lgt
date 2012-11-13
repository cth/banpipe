:- object(term_manipulation).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/06,
		comment is 'Utility predicates for term manipulation']).


	:- public(has_rule_with_head/3).
	:- info(has_rule_with_head/3, [ 
		comment is 'True if the list Terms has a rule with a given Functor and Arity',
		argnames is ['Terms','Functor','Arity']	]).
	has_rule_with_head(Terms,Functor,Arity) :-
		list::member(Rule, Terms),
		Rule =.. [ (:-), Head, _ ],
		functor(Head, Functor, Arity).

	:- public(atom_integer/2).
	:- info(atom_integer/2, [
		comment is 'True if Atom is the "same as" Integer',
		argnames is ['Atom','Integer']]).

	atom_integer(Atom,Integer) :-
        	ground(Atom),
        	atom_chars(Atom, Chars),
        	number_chars(Integer, Chars).

	atom_integer(Atom,Integer) :-
        	ground(Integer),
        	number_chars(Integer,Chars),
        	atom_chars(Atom,Chars).

	:- public(term_to_atom/2).
	:- info(term_to_atom/2, [
		comment is 'Converts ground Prolog terms to atoms.',
		argnames is ['Term','Atom']]).

	term_to_atom(TermAtom,TermAtom) :-
		atom(TermAtom), !.

	term_to_atom(Integer, Atom) :-
		integer(Integer), 
		!,
		atom_integer(Atom,Integer).

	term_to_atom(Term,Atom) :-
		Term =.. [ Functor | Arguments ],
		atom(Functor),
		findall(ArgAtom,(list::member(Arg,Arguments), term_to_atom(Arg,ArgAtom)), ArgAtoms),
		list_extras::intersperse(',',ArgAtoms,CommaSepArgAtoms),
		meta::foldl(list::append,[],[[Functor, '('], CommaSepArgAtoms, [')']],ListOfAtoms),
		meta::foldl(atom_concat,'',ListOfAtoms,Atom).

:- end_object.