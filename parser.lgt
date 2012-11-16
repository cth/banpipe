:- object(banpipe_parser).
	:- public(match_target_rule/3).
	:- public(parse_guard_and_body/3).
	:- public(parse_task_specification/4).

	%% match_target_rule(+Target,-Type,-Rule,-Index)
	:- info(match_target_rule/3,[
		comment is 'Matches a dependency rule where Target is the Nth goal of the rule head',
		argnames is ['Target','Rule','N']]).
		
	match_target_rule(Target,Rule,TargetIndex) :-
		{clause('<-'(Targets,Rule),true)},
		conjunction::nth1(TargetIndex,Targets,Target).

	:- info(parse_guard_and_body/3,[
		comment is 'parse the RHS side of a rule',
		argnames is ['RHS','Guard','Body']]).
		
	parse_guard_and_body(Spec, Guard, Body) :-
		Spec =.. [ '|', Guard, Body ],	!.
	parse_guard_and_body(Body, true, Body).

	:- info(parse_task_specification/4, [
		comment is 'Process different forms of specifying patterns for running a particular task within a module.',
		argnames is ['TaskSpec','Task','Inputs','Options']]).
		
	% case pattern example: task1([file1,file2]).
	parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
		TaskSpecification =.. [ Task, Inputs ],
		list::valid(Inputs).
	% case pattern example: task([file1,file2],[opt1(foo),opt2(bar)]).
	parse_task_specification(TaskSpecification,Task,Inputs,Options) :-
		TaskSpecification =.. [ Task, Inputs, Options ],
		list::valid(Inputs),
		list::valid(Options).
	% caase pattern example: task1(file,[opt1(foo)])
	parse_task_specification(TaskSpecification,Task,[Inputs],Options) :-
		TaskSpecification =.. [ Task, Inputs, Options ],
		not(list::valid(Inputs)),
		list::valid(Options),
		!.
	% case pattern example: task1(file1,file2).
	parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
		TaskSpecification =.. [ Task | Inputs ],
		list::valid(Inputs),
		forall(list::member(L,Inputs),not(list::valid(L))).
:- end_object.