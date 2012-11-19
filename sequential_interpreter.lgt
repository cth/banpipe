:- object(sequential_interpreter(_Semantics), implements(banpipe_interpreter)).
	:- uses(banpipe_parser, [match_target_rule/3]).
	
	:- public(run/1).
	:- info(run/1, [
		comment is 'Will run the script goal Goal using sequential bottom-up semantics (one task/process at a time). The semantics is parameterized by Semantics.',
		argnames is ['Goal']]).
		
	run(Target) :- run(Target,_).

	:- public(run/2).
	:- info(run/2, [
		comment is 'Will run the script goal Goal using sequential bottom-up semantics (one task/process at a time). The semantics is parameterized by Semantics. Result is unified to the result.',
		argnames is ['Goal','Result']]).

	run(Target,Target) :-
		atom(Target),
		atom_codes(Target,TargetSyms),
		meta::map([Atoms,Codes]>>atom_codes(Atoms,Codes),['file://','ftp://','http://', '"ftp://', '"http://'],Matchers),
		list::member(MatchSyms,Matchers),
		list::append(MatchSyms,_,TargetSyms).
		
	% run with model call body
	run(Target,Output) :-
		writeln(match_target_rule1(Target,Rule,TargetIndex)),
		match_target_rule(Target,Rule,TargetIndex),
		writeln(match_target_rule2(Target,Rule,TargetIndex)),	
		banpipe_parser::parse_guard_and_body(Rule,Guard,Body),
		{call(Guard)},
		Body =.. [ '::', Module, TaskSpec],
		banpipe_parser::parse_task_specification(TaskSpec,Task,Inputs,Options),
		self(Self),
		writeln(meta::map([I,O]>>(Self::run(I,O)),Inputs,InputsResults)),
		meta::map([I,O]>>(Self::run(I,O)),Inputs,InputsResults),
		writeln(here),
		TaskObject = task(Module,Task,InputsResults,Options),
		writeln(here2),
		parameter(1,Semantics),
		writeln(Semantics::apply(TaskObject,Outputs)),
		Semantics::apply(TaskObject,Outputs),
		writeln(here3),
		list::nth1(TargetIndex,Outputs,Output).
		
	run(Target,_File) :-
		parameter(1,Semantics),
		self(Self),
		term_extras::term_to_atom(Self,SelfAtom),
		term_extras::term_to_atom(Target,TargetAtom),
		writeln(SelfAtom),
		meta::foldl(atom_concat,'',[SelfAtom, '::run - ', 'Failed to run goal: ', TargetAtom],Error),
		reporting::error(Error).

:- end_object.