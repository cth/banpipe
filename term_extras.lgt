:- object(term_extras).
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

	:- public(vars/2).
	:- info(vars/2,[
		comment is 'Vars is the set (ordered list) of variables in Term',
		argnames is ['Term','Vars']]).
	
	vars(X,[X]) :-
		var(X), !.
		
	vars(Term,Vars) :-
		Term =.. [ _ | Arguments ],
		self(Self),
		meta::map([X,Y]>>(Self::vars(X,Y)),Arguments,VarsLists),
		list::flatten(VarsLists,VarsList),
		set::insert_all(VarsList,[],Vars).

	:- public(term_to_atom/2).
	:- info(term_to_atom/2, [
		comment is 'Converts Prolog terms to atoms.',
		argnames is ['Term','Atom']]).
		
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
		meta::map([X,Y]>>(Self::term_to_atom(X,Y,Vars)),List,ListAtoms),
		list_extras::intersperse(',',ListAtoms,CommaSepAtoms),
		meta::foldl(atom_concat,'[',CommaSepAtoms,Atom1),
		atom_concat(Atom1,']',Atom).
		
	term_to_atom(AtomTerm,Atom,_) :-
		atom(AtomTerm),
		(::atom_verbatim(AtomTerm) ->
			Atom=AtomTerm
			;
			meta::foldl(atom_concat,'',['\'',AtomTerm,'\''],Atom)).

	term_to_atom(Term,Atom,Vars) :-
		Term =.. [ Functor | Arguments ],
		atom(Functor),
		self(Self),
		meta::map([X,Y]>>(Self::term_to_atom(X,Y,Vars)),Arguments,ArgAtoms),
		list_extras::intersperse(',',ArgAtoms,CommaSepArgAtoms),
		meta::foldl(list::append,[],[[Functor, '('], CommaSepArgAtoms, [')']],ListOfAtoms),
		meta::foldl(atom_concat,'',ListOfAtoms,Atom).

	:- private(atom_verbatim/1).
	:- info(atom_verbatim/1,
		[ comment is 'True if atom is unquoted',
	          argnamas is ['Atom']]).
	
	atom_verbatim(Atom) :-
		set::insert_all("abcdefghijklmnopqrstuvwxyz_1234567890",[],WhitelistCodes),
		atom_codes(Atom,AtomCodes),
		[UnderScore] = "_",
		AtomCodes \= [UnderScore|_],
		set::insert_all(AtomCodes,[],AtomSet),
		set::subset(AtomSet,WhitelistCodes).
:- end_object.

:- object(conjunction).
	:- public(nth1/3).
	:- private(conjunction_nth/4).
	
	nth1(N,Conjunction,Element) :-
		conjunction_nth(Element,Conjunction,1,N).

	conjunction_nth(Member,(Member,_),Idx,Idx).
	conjunction_nth(Member,(ConjA,ConjB),IdxIn,IdxOut) :-
		Member \= ConjA,
		!,
		IdxNext is IdxIn + 1,
		conjunction_nth(Member,ConjB,IdxNext,IdxOut).
		conjunction_nth(Member,Member,Idx,Idx).
:- end_object.