% Dependency rule operator, note that it has same precendece as :-
:- op(1200,xfx,'<-').

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

	%% parse_guard_and_body(+Spec,-Guard,-Model,-TaskSpec)
	% parses the right-hand side (of =|<-|=) of a rule,
	% ==
	% Goal
	% or
	% Guard | Model::TaskSpec
	% or
	% Model::TaskSpec
	% == 
	% In case with no guard we assume, =|Guard=true|=.
	:- info(parse_guard_and_body/3,[
		comment is 'parse the RHS side of a rule',
		argnames is ['RHS','Guard','Body']]).
		parse_guard_and_body(Spec, Guard, Body) :-
		Spec =.. [ '|', Guard, Body ],
		!.

	parse_guard_and_body(Body, true, Body).

	%% parse_task_specification(+TaskSpec,-Task,-Inputs,-Options)
	% process different forms of specifying patterns for running a 
	% particular task within a model
	% e.g.
	% ==
	%   task1([file1,file2]).
	% ==
	parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
		TaskSpecification =.. [ Task, Inputs ],
		list::valid(Inputs).
	% or
	%   task1([file1,file2],[opt1(foo),opt2(bar)]).		
	parse_task_specification(TaskSpecification,Task,Inputs,Options) :-
		TaskSpecification =.. [ Task, Inputs, Options ],
		list::valid(Inputs),
		list::valid(Options).
	% or 
	% 	task1(file,[opt1(foo)])
	parse_task_specification(TaskSpecification,Task,[Inputs],Options) :-
		TaskSpecification =.. [ Task, Inputs, Options ],
		not(list::valid(Inputs)),
		list::valid(Options),
		!.
	% or
	%    task1(file1,file2).
	parse_task_specification(TaskSpecification,Task,Inputs,[]) :-
		TaskSpecification =.. [ Task | Inputs ],
		list::valid(Inputs),
		forall(list::member(L,Inputs),not(list::valid(L))).
:- end_object.

% This one possibly needs to be parameterized
% We need to figure out how to invoke the task
% For now, it will just be a stub
:- object(banpipe_invoker).
	:- public(run_task/3).
	
	run_task(Module,RealTaskSpec,RunTaskOptions) :-
		writeln(run_task(Module,RealTaskSpec,RunTaskOptions)).
:- end_object.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OBJECT: banpipe_interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- object(banpipe_interpreter(sequential)).
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
	run(Target,RunOpts,File) :-
		match_target_rule(Target,Rule,TargetIndex),
		banpipe_parser::parse_guard_and_body(Rule,Guard,Body),
		call(Guard),
		Body =.. [ '::', Module, TaskSpec],
		banpipe_parser::parse_task_specification(TaskSpec,Task,Inputs,Options),
		run_options(RunOpts,RunTaskOptions,NewRunOpts),
		run_multiple(NewRunOpts,Inputs,InputFiles),
		/*
		RealTaskSpec =.. [ Task, InputFiles, Options, OutputFiles ],
		banpipe_invoker::run_task(Module,RealTaskSpec,RunTaskOptions),
		%run_model(Model,RealTaskSpec,RunModelOptions),
		*/
		task(Module,Task,InputFiles,Options)::run(OutputFiles),
		list::nth1(TargetIndex,OutputFiles,File).
		
	run(Target,_RunOpts,_File) :-
		write('BANpipe error. Could not run target: '),
		writeln(Target),
		%debug(script(run),['failed to run target: ', Target]),
		!,
		fail.

	run_multiple(_,[],[]).
	run_multiple(RunOpts,[Target|TargetsRest],[File|FilesRest]) :-
		run(Target,RunOpts,File),
		run_multiple(RunOpts,TargetsRest,FilesRest).
:- end_object.


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
		config::execution_mode(Mode),
		banpipe_interpreter(Mode)::run(Goal).
:- end_object.