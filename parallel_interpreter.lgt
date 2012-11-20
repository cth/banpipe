:- object(parallel_interpreter(Semantics), extends(banpipe_interpreter)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/19,
		comment is 'Interpreter for parallel execution of banpipe scripts.']).

		%% run_parallel(+Goal)
		% Runs the Goal using the parallel semantics. 
		run_parallel(Target) :-
			generate_call_specs(Target,[],CallSpecs),
			writeln(CallSpecs),
			writeln('-------------'),
			build_scheduler_tree(CallSpecs,SchedulerTree),
			writeln('-------------'),
			scheduler_tree_print(SchedulerTree),!,
			writeln('reducing tree'),nl,
			scheduler_tree_reduce(SchedulerTree,ReducedTree),!,
			scheduler_tree_print(ReducedTree),
			scheduler_init,
			scheduler_loop(ReducedTree,[]),
			scheduler_shutdown.

		simplify_goal(Goal,Simplified) :-
			Goal =.. [ Functor, InFiles, Opts, OutFile ],
			file_base_name(OutFile,SimplerOutFile),
			map(file_base_name,InFiles,SimplerInFiles),
			Simplified =.. [ Functor, SimplerInFiles, Opts, SimplerOutFile ].

		build_scheduler_tree(CallSpec,FinalTree) :-
			scheduler_tree_create(EmptyTree),
			build_scheduler_tree_rec(nil,CallSpec,EmptyTree,FinalTree).

		build_scheduler_tree_rec(_,[],Tree,Tree).

		build_scheduler_tree_rec(_,[(nil,nil,_)],Tree,Tree).

		build_scheduler_tree_rec(Parent,[(Model,Goal,_),ChildCalls],InTree,OutTree) :-
			scheduler_tree_add(Model,Goal,Parent,InTree,OutTree1,TaskId),
			build_scheduler_tree_rec(TaskId,ChildCalls,OutTree1,OutTree).

		build_scheduler_tree_rec(Parent,[[(Model,Goal,_),Children]|Siblings],InTree,OutTree) :-
			scheduler_tree_add(Model,Goal,Parent,InTree,Tree1,TaskId),
			build_scheduler_tree_rec(TaskId,Children,Tree1,Tree2),
			build_scheduler_tree_rec(Parent,Siblings,Tree2,OutTree).

		generate_call_specs(lost_data_file(Identifier),_RunOpts,(nil,nil,File)) :-
			atom(Identifier),
			lost_data_file(Identifier,File).

		generate_call_specs(file(File),_RunOpts,(nil,nil,File)) :-
			atom(File),
			% Check that file exists
			(file_exists(File) ->
				true
				;
				atom_concat_list(['File ', File, ' does not exist!'],ErrMsg),
				throw(ErrMsg)).

		% Run with procotol identifiers
		generate_call_specs(Target,_RunOpts,(nil,nil,File)) :-
			atom(Target),
			atom_codes(Target,TargetSyms),
			atom_codes('file://', MatchSyms),
			append(MatchSyms,FileCodes,TargetSyms),
			atom_codes(File,FileCodes).

		generate_call_specs(Target,_RunOpts,(nil,nil,Target)) :-
			atom(Target),
			atom_codes(Target,TargetSyms),
			map(atom_codes,['ftp://','http://', '"ftp://', '"http://'],Matchers),
			member(MatchSyms,Matchers),
			append(MatchSyms,_,TargetSyms).

		generate_call_specs(Target,RunOpts,[(Model,RunGoal,OutputFiles),ChildSpecs]) :-
			clause('<-'(Target,Rule),true),
			parse_guard_and_body(Rule,Guard,Model,TaskSpec),
			call(Guard), % Make sure that the guard holds
			parse_task_specification(TaskSpec,Task,Inputs,Options),
			run_options(RunOpts,_RunModelOptions,NewRunOpts),
			findall([(ChildModel,ChildGoal,ChildOutputFile),SubSpecs],
				(
					member(Dependency,Inputs),
					generate_call_specs(Dependency,NewRunOpts,[(ChildModel,ChildGoal,ChildOutputFiles),SubSpecs])
				),
				ChildSpecs),
			map(callspec_output_file,ChildSpecs,InputFiles),
			RunGoal =.. [ Task, InputFiles, Options, OutputFiles ],
			goal_result_files(Model,RunGoal,OutputFiles). % FIXME


:- end_object.
