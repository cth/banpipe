:- object(sequential_interpreter(_Semantics), implements(banpipe_interpreter)).
	:- uses(banpipe_parser, [match_target_rule/3]).
	
	:- public(run/1).
	:- info(run/1, [
		comment is 'Will run the script goal Goal using sequential bottom-up semantics (one task/process at a time). The semantics is parameterized by Semantics.',
		argnames is ['Goal']]).
		
	run(Target) :- ::run(Target,_).

	:- public(run/2).
	:- info(run/2, [
		comment is 'Same as run/1, but additionally Result is unified to the result file/type/... (depending on Semantics parameterization).',
		argnames is ['Goal','Result']]).
		
	run(Goal,Result) :-
		::run(Goal,Result,_).
		
	:- public(run/3).
	:- info(run/3, [
		comment is 'Same as run/2, but additionally generates a trace',
		argnames is ['Goal','Result','Trace']]).
	
	%run(A,B,C) :-
	%	writeln(run(A,B,C)),
	%	fail.

	run(Target,Target,(nil,[Target])) :-
		uri(Target)::valid.

	% run with model call body
	run(Target,Output,[(TaskObject,Outputs),ChildSpecs]) :-
		match_target_rule(Target,LHS+RHS,TargetIndex),
		banpipe_parser::parse_guard_and_body(RHS,Guard,Body),
		{call(Guard)},
		Body =.. [ '::', Module, TaskSpec],
		banpipe_parser::parse_task_specification(TaskSpec,Task,Inputs,Options),
		self(Self),
		meta::map([I,[O,C]]>>(Self::run(I,O,C)),Inputs,OutputsAndChildSpecs),
		meta::map([[O,_],O]>>true,OutputsAndChildSpecs,InputsResults),
		meta::map([[_,C],C]>>true,OutputsAndChildSpecs,ChildSpecs),
		TaskObject = task(Module,Task,InputsResults,Options),
		parameter(1,Semantics),
		Semantics::apply(LHS+RHS,TaskObject,Outputs),
		list::nth1(TargetIndex,Outputs,Output).
		
	run(Target,_Output,_Trace) :-
		self(Self),
		term_extras::term_to_atom(Self,SelfAtom),
		term_extras::term_to_atom(Target,TargetAtom),
		writeln(SelfAtom),
		meta::foldl(atom_concat,'',[SelfAtom, '::run - ', 'Failed to run goal: ', TargetAtom],Error),
		reporting::error(Error).
:- end_object.