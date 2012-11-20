:- protocol(task_semantics).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'Protocol for applying a particular semantics to a task.']).
	
	:- public(apply/3).
	:- info(apply/3, [
		comment is 'Apply semantics for Task to obtain Result',
		argnames is ['Rule','Task','Result']]).
:- end_protocol.


:- object(execution_semantics, implements(task_semantics)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'A semantics for tasks, which computes output files.']).
		
	apply(_,Task,Result) :-
		Task::run(Result).
:- end_object.

:- object(typecheck_semantics, implements(task_semantics)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'A semantics for tasks, which performs type checking.']).
		
	apply(_,Task,Result) :-
		Task::typecheck(Result).
:- end_object.

:- object(trace_semantics, implements(task_semantics)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/19,
		comment is 'Semantics used for generating call trace. Only the syntactical Rule is relevant.']).
		
	apply(LHS+RHS,_Task,LHS).
:- end_object.

