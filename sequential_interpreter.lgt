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
		match_target_rule(Target,Rule,TargetIndex),
		banpipe_parser::parse_guard_and_body(Rule,Guard,Body),
		call(Guard),
		Body =.. [ '::', Module, TaskSpec],
		banpipe_parser::parse_task_specification(TaskSpec,Task,Inputs,Options),
		meta::map([I,O]>>(::run(I,O)),Inputs,InputsResults),
		TaskObject = task(Module,Task,InputsResults,Options),
		parameter(1,Semantics),
		Semantics::apply(TaskObject,Outputs),
		list::nth1(TargetIndex,Outputs,Output).
		
	run(Target,_File) :-
		parameter(1,Semantics),
		meta::foldl(atom_concat,'',[sequential_interpreter(Semantics), '::run - ', 'Failed to run goal: ', Target],Error),
		reporting::error(Error).
:- end_object.