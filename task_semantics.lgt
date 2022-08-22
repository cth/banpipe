:- protocol(task_semantics).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-16,
		comment is 'Protocol for applying a particular semantics to a task.'
	]).
	
	:- public(apply/3).
	:- info(apply/3, [
		comment is 'Apply semantics for Task to obtain Result',
		argnames is ['Rule','Task','Result']
	]).

:- end_protocol.


:- object(execution_semantics, implements(task_semantics)).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-16,
		comment is 'A semantics for tasks, which computes output files.'
	]).
		
	apply(_,Task,Result) :-
		Task::run(Result).

:- end_object.


:- object(typecheck_semantics, implements(task_semantics)).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-16,
		comment is 'A semantics for tasks, which performs type checking.'
	]).
		
	apply(_,Task,Result) :-
		Task::typecheck(Result).

:- end_object.


:- object(callgraph_semantics, implements(task_semantics)).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-19,
		comment is 'Semantics used for generating call graph. Only the syntactical Rule is relevant.'
	]).

	apply(LHS+_RHS,_Task,LHS).

:- end_object.


:- object(trace(_Semantics), implements(task_semantics)).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-19,
		comment is 'A Semantics which implements a very simple tracer',
		parnames is ['Semantics']
	]).

	apply(LHS+RHS,Task,Outputs) :-
		write('call: '),
		write(LHS),
		write(' <- '),
		write(RHS), nl,
		write('(c)reep (a)bort>'), nl,
		get_char(Action),
		perform_action(Action,LHS+RHS,Task,Outputs).
		
	:- private(perform_action/4).
		
	perform_action(a,_,_,_) :-
		!,
		throw(tracer(abort)).	
		
	% creep
	perform_action(_,Rule,Task,Outputs) :-
		write(creep), nl,
		parameter(1,Semantics),
		Semantics::apply(Rule,Task,Outputs),
		write('-->'), write(Outputs), nl.
:- end_object.

