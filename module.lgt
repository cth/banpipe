:- object(banpipe_module_path).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/09,
		comment is 'Manages the module directory search paths. These are obtained from the BANPIPE_MODULE_PATH and directories which have subsequently programmatically been added through the include_directory predictate.']).
		
	:- private(path_dir/1).
	:- dynamic(path_dir/1).

	:- public(get_paths/1).
	:- info(get_paths/1,
		[ comment is 'Extract module directories from the BANPIPE_MODULE_PATH environment variable.',
		  argnames is ['Paths']]).
	get_paths(Paths) :-
		(shell::environment_variable('BANPIPE_MODULE_PATH',PathAtom) ->
			[Separator] = ":",
			atom_codes(PathAtom,PathCodes),	
			list_extras::sublist_split(Separator,PathCodes,PathsCodes),
			meta::map([X,Y]>>atom_codes(Y,X),PathsCodes,EnvPaths)
			;
			EnvPaths = []),
		findall(Dir,::path_dir(Dir),AdditionalDirs),
		list::append(AdditionalDirs,EnvPaths,Paths).
		
	:- public(include_directory/1).
	:- info(include_directory/1,
		[ comment is 'Programmatically appends a directory to the module search path',
		  argnames is ['Directory']]).
	include_directory(Directory) :-
		(::path_dir(Directory) -> 
			true
			;
			::assertz(path_dir(Directory))).
:- end_object.

:- object(module(_Name)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/09,
		comment is 'A proxy object which represent a particular banpipe module.']).

	:- public(interface_file/1).
	
	interface_file(File) :-
		parameter(1,ModuleName),
		banpipe_module_path::get_paths(Paths),
		list::member(Path,Paths),
		meta::foldl(atom_concat,'',[Path, '/', ModuleName, '/', 'interface.pl'],File),
		file(File)::exists.
:- end_object.

:- object(module_task(_Module,_Task)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/09,
		comment is 'Represents a task in a module']).

	:- public(valid/1).
	:- info(valid/1,
		[ comment is 'Checks if a call with option parameters Options to the task represented by the object is valid. The module+task must exist, have a declaration and a implementation in the interface file and the options must match (be a subset of) declared options of the task.',
		  argnames is ['Options']]).
	valid(Options) :-
		parameter(1,Module),
		parameter(2,Task),
		% Check that the task is declared
		(::has_declaration -> true ; reporting::error(no_task_declaration(Module,Task))),
		(::has_implementation -> true ; reporting::error(no_task_implementation(Module,Task))),
		::options(DeclaredOptions),
		(forall(list::member(Opt,Options), (Opt =.. [F,_], MatchOpt =.. [F,_],list::member(MatchOpt,DeclaredOptions))) ->
			true
			;
			reporting::error(interface(model_called_with_undeclared_options(Module,Options)))).
			
	:- public(expand_options/2).
	:- info(expand_options/2,
		[ comment is 'Given "call options" Options are expanded to SortedExpandedOptions - A sorted list which include all Options and all declared default options, except if an option in Options use same functor. ',
		  argnams is [ 'Options', 'SortedExpandedOptions' ]]).
	expand_options(Options,SortedExpandedOptions) :-
		::valid(Options),
		::options(DefaultOptions),
		meta::map([OptIn,OptOut]>>(OptIn=..[F,_],OptOut=..[F,_],(list::member(OptOut,Options) -> true ; OptOut=OptIn)), DefaultOptions, ExpandedOptions),
		list::sort(ExpandedOptions,SortedExpandedOptions).
		
/*
	:- public(invoke/2).
	invoke(Invoker,InputFiles,Options,OutputFiles) :-
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(InterfaceFile),
		::expand_options(Options,ExpandedOptions),
		list::length()
		Goal =.. [ Task, InputFiles, ExpandedOptions, OutputFiles ],
		Invoker::run(InterfaceFile,),
*/		
	
	:- private(has_declaration/0).
	:- info(has_declaration/0, [comment is 'True if the task has a declaration.']).
	has_declaration :-
		::declaration(_).

	:- private(has_implementation/0).
	:- info(has_implementation/0, [comment is 'True if the there is an implementation of the task. This merely verifies that a predicate of the correct name and arity exists, but not whether it works.']).
	has_implementation :-
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(File),
		prolog_file(File)::read_terms(Terms),
		term_extras::has_rule_with_head(Terms,Task,3).

	:- private(declaration/1).
	:- info(declaration/1,
		[ comment is 'Declaration is the task declation, e.g, taskname(input_types,options,output_types).',
		argnames is ['Declaration']]).
	declaration(Declaration) :-
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(InterfaceFile),
		prolog_file(InterfaceFile)::member(TaskMatcher),
		TaskMatcher =.. [ ':-', task(Declaration) ],
		Declaration =.. [ Task | _ ].
		
	:- public(options/1).
	:- info(options/1, 
		[ comment is 'Options is the list of declared options for task.',
		  argnames is ['Options']]).
	options(Options) :-
		declaration(Decl),
		Decl =.. [ _ , _, Options, _ ].
	
	:- public(input_types/1).
	:- info(input_types/1,
		[ comment is 'InputTypes is a sequence of file types accepted as input to the task.',
		  argnames is ['InputTypes']]).
	input_types(InputTypes) :-
		declaration(Decl),
		Decl =.. [ _ , InputTypes, _, _ ].

	:- public(output_types/1).
	:- info(output_types/1,
		[ comment is 'OutputTypes is a sequence of file types produced by the task.',
		  argnames is ['OutputTypes']]).
	output_types(OutputTypes) :-
		declaration(Decl),
		Decl =.. [ _ , _, _, OutputTypes ].
		

:- end_object.

:- object(task(Module,Task,_InputFiles,_Options), extends(module_task(Module,Task))).
	:- public(run/1).
	:- info(run/1,[
		comment is 'Runs the task(Module,Task,InputFiles,Options) using an invoker (see invoker/1) if the task is not available on file(s). In either case, OutputFiles as a list of names of resulting files.',
		argnames is ['OutputFiles']]).
		
	% FIXME: hook into logging
	run(OutputFiles) :-
		::output_types(OutputTypes),
		list::length(OutputTypes,N),
		list::length(OutputFiles,N),
		parameter(1,Module),
		parameter(2,Task),
		parameter(3,InputFiles),
		parameter(4,Options),
		config::get(file_manager,FileManager),
		::expand_options(Options,ExpandedOptions),
		(FileManager::result_files(Module,Task,InputFiles,ExpandedOptions,OutputFiles) ->
			true % The task has allready been run 
			;
			FileManager::result_files_allocate(Module,Task,InputFiles,ExpandedOptions,OutputFiles),
			::invoker(Invoker),
			module(Module)::interface_file(InterfaceFile),
			Goal =.. [ Task, InputFiles, ExpandedOptions, OutputFiles ],
			Invoker::run(InterfaceFile,Goal),!,
			FileManager::result_files_commit(Module,Task,InputFiles,ExpandedOptions)).
		
	:- public(invoker/1).
	:- info(invoker/1,
		[ comment is 'Determines which InvokerObject to use to execute the task. If there is a override_invoker config directive, then that it used (e.g. a type checker), second if module itself declares a particular invoker to use, then that is used. Otherwise the invoker indicated by the config directive default_invoker is used',
		  argnames is ['InvokerObject']]).
% Should not be necessary
%	invoker(InvokerName) :-
%		config::get(override_invoker,InvokerName),
%		!.
	invoker(InvokerName) :-
		parameter(1,Module),
		module(Module)::interface_file(InterfaceFile),
		prolog_file(InterfaceFile)::member(TaskMatcher),
		TaskMatcher =.. [ ':-', invoke_with(PrologName) ],
		atom_concat(PrologName,'_invoker',InvokerName),
		!.
	invoker(Invoker) :-
		config::get(default_invoker,Invoker).
		
	% TODO: Eventually, I might move guard invokation to run/1 (rather than in script interpreter)
	% to allow unifying variables from the default option settings
:- end_object.