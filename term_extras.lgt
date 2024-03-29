:- object(term_extras).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-06,
		comment is 'Utility predicates for term manipulation'
	]).

	:- uses(list, [
		valid/1 as is_list/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- public(has_rule_with_head/3).
	:- info(has_rule_with_head/3, [ 
		comment is 'True if the list Terms has a rule with a given Functor and Arity',
		argnames is ['Terms','Functor','Arity']
	]).

	has_rule_with_head(Terms,Functor,Arity) :-
		list::member(Rule, Terms),
		Rule = (Head :- _),
		functor(Head, Functor, Arity).

	:- public(atom_integer/2).
	:- info(atom_integer/2, [
		comment is 'True if Atom is the "same as" Integer',
		argnames is ['Atom','Integer']
	]).

	atom_integer(Atom,Integer) :-
		ground(Atom),
		atom_chars(Atom, Chars),
		number_chars(Integer, Chars).

	atom_integer(Atom,Integer) :-
		ground(Integer),
		number_chars(Integer,Chars),
		atom_chars(Atom,Chars).

	:- public(vars/2).
	:- info(vars/2,[
		comment is 'Vars is the set (ordered list) of variables in Term',
		argnames is ['Term','Vars']
	]).

	vars(X,[X]) :-
		var(X), !.

	vars(Term,Vars) :-
		Term =.. [ _ | Arguments ],
		self(Self),
		meta::map({Self}/[X,Y]>>(Self::vars(X,Y)),Arguments,VarsLists),
		list::flatten(VarsLists,VarsList),
		set::insert_all(VarsList,[],Vars).

	:- public(term_to_atom/2).
	:- info(term_to_atom/2, [
		comment is 'Converts Prolog terms to atoms.',
		argnames is ['Term','Atom']
	]).

	term_to_atom(Term,Atom) :-
		vars(Term,Vars),
		term_to_atom(Term,Atom,Vars).

	:- public(term_to_atom/3).
	term_to_atom(X,Atom,Vars) :-
		var(X),
		!,
		list::nth1(N,Vars,X),
		term_to_atom(N,AtomN),
		atom_concat('V',AtomN,Atom).


	term_to_atom(Number, Atom, _) :-
		number(Number),
		!,
		atom_integer(Atom,Number).

	term_to_atom(List,Atom,Vars) :-
		is_list(List),
		!,
		self(Self),
		meta::map({Self}/[X,Y]>>(Self::term_to_atom(X,Y,Vars)),List,ListAtoms),
		list_extras::intersperse(',',ListAtoms,CommaSepAtoms),
		meta::foldl(atom_concat,'[',CommaSepAtoms,Atom1),
		atom_concat(Atom1,']',Atom).

	term_to_atom(AtomTerm,Atom,_) :-
		atom(AtomTerm),
		(::atom_verbatim(AtomTerm) ->
			Atom=AtomTerm
			;
			atomic_list_concat(['\'',AtomTerm,'\''],Atom)).

	term_to_atom(Term,Atom,Vars) :-
		Term =.. [ Functor | Arguments ],
		atom(Functor),
		self(Self),
		meta::map({Self}/[X,Y]>>(Self::term_to_atom(X,Y,Vars)),Arguments,ArgAtoms),
		list_extras::intersperse(',',ArgAtoms,CommaSepArgAtoms),
		meta::foldl(list::append,[],[[Functor, '('], CommaSepArgAtoms, [')']],ListOfAtoms),
		meta::foldl(atom_concat,'',ListOfAtoms,Atom).

	:- private(atom_verbatim/1).
	:- info(atom_verbatim/1, [
		comment is 'True if atom is unquoted',
		argnamas is ['Atom']
	]).

	atom_verbatim(Atom) :-
		atom_codes('abcdefghijklmnopqrstuvwxyz_1234567890', WhitelistCodes0),
		set::as_set(WhitelistCodes0, WhitelistCodes),
		atom_codes(Atom,AtomCodes),
		AtomCodes \= [0'_|_],
		set::insert_all(AtomCodes,[],AtomSet),
		set::subset(AtomSet,WhitelistCodes).

	:- public(conjunction_as_list/2).
	:- info(conjunction_as_list/2,[
		comments is 'Converversion from Conjunction to List, i.e., (A,B,C) => [A,B,C].',
		argnames is ['Conjunction','List']
	]).

	conjunction_as_list((A,ConjRest),[A|ListRest]) :-
		!,
		conjunction_as_list(ConjRest,ListRest).
	conjunction_as_list(X,[X]).

:- end_object.
