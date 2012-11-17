:- protocol(task_semantics).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'Protocol for applying a particular semantics to a task.']).
	
	:- public(apply/2).
	:- info(apply/2, [
		comment is 'Apply semantics for Task to obtain Result',
		argnames is ['Task','Result']]).
:- end_protocol.


:- object(execution_semantics(_Task), implements(task_semantics)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'A semantics for tasks, which computes output files.']).
		
	apply(Task,Result) :-
		Task::run(Result).
:- end_object.

:- object(typecheck_semantics(_Task), implements(task_semantics)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'A semantics for tasks, which computes output files.']).
		
	apply(Task,Result) :-
		Task::typecheck(Result).
:- end_object.

:- object(sequential_interpreter(Semantics), extends(banpipe_interpreter)).
	:- uses(banpipe_parser, [match_target_rule/3]).
	
	:- public(run/1).
	
	run_options([rerun(recursive)],[caching(false)],[rerun(recursive)]).
	run_options([rerun(once)],[caching(false)],[]).
	run_options([],[caching(true)],[]).

	%% run(+Goal)
	% Will run the script goal Goal with the sequential semantics (one task/process at a time).  
	run(Target) :-
		run(Target,[]).

	run(Target,Opts) :-
		run(Target,Opts,_File).

	% Run with procotol identifiers
	run(Target,_RunOpts,File) :-
		atom(Target),
		atom_codes(Target,TargetSyms),
		atom_codes('file://', MatchSyms),
		list::append(MatchSyms,FileCodes,TargetSyms),
		atom_codes(File,FileCodes).
	
	run(Target,_RunOpts,Target) :-
		atom(Target),
		atom_codes(Target,TargetSyms),
		meta::map([Atoms,Codes]>>atom_codes(Atoms,Codes),['ftp://','http://', '"ftp://', '"http://'],Matchers),
		list::member(MatchSyms,Matchers),
		list::append(MatchSyms,_,TargetSyms).

	% run with model call body
	run(Target,RunOpts,Output) :-
		match_target_rule(Target,Rule,TargetIndex),
		banpipe_parser::parse_guard_and_body(Rule,Guard,Body),
		call(Guard),
		Body =.. [ '::', Module, TaskSpec],
		banpipe_parser::parse_task_specification(TaskSpec,Task,Inputs,Options),
		::run_options(RunOpts,RunTaskOptions,NewRunOpts),
		::run_multiple(NewRunOpts,Inputs,InputFiles),
		TaskObject = task(Module,Task,InputFiles,Options),
		Semantics::apply(TaskObject,Outputs), % TODO: Fix 
		list::nth1(TargetIndex,Outputs,Output).
		
	run(Target,_RunOpts,_File) :-
		write('BANpipe error. Could not run target: '),
		writeln(Target),
		!,
		fail.

	run_multiple(_,[],[]).
	run_multiple(RunOpts,[Target|TargetsRest],[File|FilesRest]) :-
		::run(Target,RunOpts,File),
		::run_multiple(RunOpts,TargetsRest,FilesRest).
:- end_object.