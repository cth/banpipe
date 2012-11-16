% Dependency rule operator, note that it has same precendece as :-
:- op(1200,xfx,'<-').



:- object(banpipe).
	:- public(listing/0).
	:- public(listing/1).
	:- public(run/1).
	:- public(load/1).
	
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/13,
		comment is 'The main object for interaction with banpipe scripts.']).

	:- dynamic('<-'/2).
	
	:- info(listing/1, 
		[ comment is 'Lists all banpipe dependency rules where the goal G occurs in the head.',
		argnames is ['G']]).
	listing(Goal) :-
		% escaped using {}/1 operator to find clause from "global" database rather than this objects database
		findall([Head,Body],({clause('<-'(Head,Body),true)},conjunction::nth1(_,Head,Goal)),Rules),
		forall(list::member([Head,Body],Rules),(write(Head), write(' <- '), writeln(Body))).

	:- info(listing/0,
		[ comment is 'Lists all banpipe dependency rules.']).
	listing :- listing(_).
	
	% This simply loads a script using Prologs normal mechanism
	load(Script) :- {[Script]}.
	
	run(Goal) :-
		config::get(execution_mode,Mode),
		banpipe_interpreter(Mode)::run(Goal).
		
	typecheck(Goal) :-
		type_checker::run(Goal).
		
:- end_object.