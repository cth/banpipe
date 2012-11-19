% Dependency rule operator, note that it has same precendece as :-
:- op(1200,xfx,'<-').

:- object(banpipe).

	:- public(load/1).
	
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/13,
		comment is 'The main object for interaction with banpipe scripts.']).

	:- dynamic('<-'/2).
	
	:- public(listing/1).
	:- info(listing/1, 
		[ comment is 'Lists all banpipe dependency rules where the goal G occurs in the head.',
		argnames is ['G']]).
	listing(Goal) :-
		% escaped using {}/1 operator to find clause from "global" database rather than this objects database
		findall([Head,Body],({clause('<-'(Head,Body),true)},conjunction::nth1(_,Head,Goal)),Rules),
		forall(list::member([Head,Body],Rules),(write(Head), write(' <- '), writeln(Body))).

	:- public(listing/0).
	:- info(listing/0,
		[ comment is 'Lists all banpipe dependency rules.']).
	listing :- listing(_).
	
	% This simply loads a script using Prologs normal mechanism
	:- info(load/1, [
		comment is 'Loads a banpipe script. Script is the (quoted) filename of the script (absolute or relative to current directory).',
		argnames is ['Script']]).
	load(Script) :- {[Script]}.
	
	:- public(run/1).
	:- info(run/1, [
		comment is 'Recursively compute the file associated with Goal.',
		argnames is ['Goal']]).
	run(Goal) :-
		::run(Goal,_).

	:- public(run/2).
	:- info(run/2, [
		comment is 'Recursively compute the File associated with Goal.',
		argnames is ['Goal','File']]).

	run(Goal,Result) :-
		config::get(execution_mode,Mode),
		((Mode == sequential) ->
			sequential_interpreter(execution_semantics)::run(Goal,Result)
			;
		((Mode == parallel) ->
			%parallel_interpreter::run(Goal)
			reporting::error('parallel_interpreter missing')
			;
			reporting::error(no_such_execution_mode(Mode)))),
		!.
			
	:- public(typecheck/1).
	:- info(typecheck/1,[
		comment is 'Typecheck Goal -- recursively check that the types of input files for Goal are compatible.',
		argnames is ['Goal']]).

	typecheck(Goal) :-
		typecheck(Goal,_).
		
	:- public(typecheck/2).
	:- info(typecheck/2,[
		comment is 'Typecheck Goal -- recursively check that the types of input files for Goal are compatible and unify resulting Type.',
		argnames is ['Goal','Type']]).
	typecheck(Goal,Type) :-
		sequential_interpreter(typecheck_semantics)::run(Goal,Type), !.
:- end_object.