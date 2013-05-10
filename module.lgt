:- object(banpipe_module_path).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/09,
		comment is 'Manages the module directory search paths.',
		remarks is [ 
			'environment_variable $BANPIPE_MODULE_PATH'  - 'Search paths are obtained from the BANPIPE_MODULE_PATH (paths separated by : (unix) or ; (windows)',
			'explicit paths in scripts' - 'module directories can be added explicitly from scripts, see include_directory/1']
			]).
		
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
			meta::map([X,Y]>>(expand_path(X,ExpandPath),atom_codes(Y,ExpandPath)),PathsCodes,EnvPaths)
			;
			EnvPaths = []),
		findall(Dir,::path_dir(Dir),AdditionalDirs),
		meta::map([X,Y]>>(atom_codes(X,DirC),expand_path(DirC,ExpandDirC),atom_codes(Y,ExpandDirC)),AdditionalDirs,ExpandAdditionalDirs), 
		list::append(ExpandAdditionalDirs,EnvPaths,Paths).

	:- private(expand_path/2).
	:- info(expand_path/2, [comment is 'Expand relative paths starting with . or not with /']).

	% Windows style absolute path with drive letter
	expand_path(Path,Path) :-
		atom_codes(':',Colon),
		Path = [_DriveLetter,Colon|_],
		!.

	% Unix style absolute path (starts with /)
	expand_path(Path,Path) :- 
		atom_codes('/',Slash),
		Path = [ Slash | _ ],
		!.

	% Otherwise, we assume that it is a local relative path
	expand_path(Path,ExpandPath) :-
		shell::working_directory(AbsDir),
		atom_codes(AbsDir,AbsDirCodes),
		list::append(AbsDirCodes,Path,ExpandPath).
		
	:- public(check_set/0).
	:- info(check_set/0, [comment is 'Check that BANPIPE_MODULE_PATH is set.']).
	check_set :-
		report_unless(shell::environment_variable('BANPIPE_MODULE_PATH',_))::warning('BANPIPE_MODULE_PATH not set.').
		
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
		comment is 'A proxy object which represent a particular banpipe module.',
		parnames is ['Name']
	]).

	:- public(interface_file/2).
	
	interface_file(File,prolog) :-
		parameter(1,ModuleName),
		banpipe_module_path::get_paths(Paths),
		list::member(Path,Paths),
		meta::foldl(atom_concat,'',[Path, '/', ModuleName, '/', 'interface.pl'],File),
		file(File)::exists.
	
	interface_file(File,generic) :-
		parameter(1,ModuleName),
		banpipe_module_path::get_paths(Paths),
		list::member(Path,Paths),
		meta::foldl(atom_concat,'',[Path, '/', ModuleName, '/', 'interface.banpipe'],File),
		file(File)::exists.

	:- public(builtin/0). 
	:- info(builtin/0,[comment is 'True if module is a builtin module']).
	builtin :-
		parameter(1,Module),
		implements_protocol(Module,banpipe_builtin_module).

	:- public(module_type/1). 
	:- info(module_type/1,[comment is 'True if module is a builtin module']).
	module_type(builtin) :-
		self(Self),
		Self::builtin.

	module_type(prolog) :-
		interface_file(_,prolog).

	module_type(generic) :-
		interface_file(_,generic).

		
	:- public(available/0).
	:- info(available/0,[comment is 'True if a module with the given name is available.']).
	available :-
		::builtin 
		;
		::interface_file(_,_).
:- end_object.

:- object(module_task(_Module,_Task)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/09,
		comment is 'Represents a task in a module',
		parnames is ['Module', 'Task']
	]).

	:- public(valid/1).
	:- info(valid/1,
		[ comment is 'True if module+task exists, have a declaration+implementation in the interface file and Options matches (is a subset of) declared options.',
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
		[ comment is 'Options are expanded to SortedExpandedOptions: A sorted list which include Options+declared default options, except if an option in Options use same functor. ',
		  argnams is [ 'Options', 'SortedExpandedOptions' ]]).
	expand_options(Options,SortedExpandedOptions) :-
		::options(DefaultOptions),
		::expand_options(Options,DefaultOptions,SortedExpandedOptions).

	:- public(expand_options/3).
	expand_options(Options,DefaultOptions,SortedExpandedOptions) :-
		::valid(Options),
		meta::map([OptIn,OptOut]>>((OptIn=..[F,X],OptOut=..[F,X], list::member(OptOut,Options) -> true
						; (OptIn=..[F,_],OptOut=..[F,_],list::member(OptOut,Options)) -> true
							; OptOut=OptIn)),
			DefaultOptions,ExpandedOptions),
		list::sort(ExpandedOptions,SortedExpandedOptions).

	:- public(has_declaration/0).
	:- info(has_declaration/0, [comment is 'True if the task has a declaration.']).
	has_declaration :-
		::declaration(_).

	:- public(has_implementation/0).
	:- info(has_implementation/0, [comment is 'True if the there is an implementation predicate for the task.']).

	% has_implementation/0 for built-in modules
	has_implementation :-
		parameter(1,Module),
		module(Module)::builtin,
		!,
		parameter(2,Task),
		TaskGoal =.. [ Task, _, _, _ ],
		Module::predicate_property(TaskGoal,(public)).
	
	% has_implementation/0 for external prolog-based banpipe modules
	has_implementation :-
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(File,prolog),
		prolog_file(File)::read_terms(Terms),
		term_extras::has_rule_with_head(Terms,Task,3).

	% has_implementation/0 for external generic banpipe modules
	% In this case we do not check for implementation (implementation is program/shell command)
	has_implementation :-
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(File,generic),
		prolog_file(File)::member(invoke(Task,_Method)).

	:- public(declaration/1).
	:- info(declaration/1,
		[ comment is 'Declaration is the task declation, e.g, taskname(input_types,options,output_types).',
		argnames is ['Declaration']]).
	declaration(Declaration) :-
		parameter(1,Module),
		module(Module)::builtin,
		!,
		parameter(2,Task),
		Module::task(Declaration),
		Declaration =.. [ Task | _ ].
		
	declaration(Declaration) :-
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(InterfaceFile,prolog),
		prolog_file(InterfaceFile)::member(TaskMatcher),
		TaskMatcher =.. [ ':-', task(Declaration) ],
		Declaration =.. [ Task | _ ].

	declaration(Declaration) :- 
		parameter(1,Module),
		parameter(2,Task),
		module(Module)::interface_file(InterfaceFile,generic),
		prolog_file(InterfaceFile)::member(TaskMatcher),
		TaskMatcher = task(Declaration), 
		Declaration =.. [ Task | _ ].
		
	:- public(options/1).
	:- info(options/1, 
		[ comment is 'Options is the list of declared options for task.',
		  argnames is ['Options']]).
	options(Options) :-
		::declaration(Decl),
		Decl =.. [ _ , _, OptionsDecl, _ ],
		(list::member(version(_),OptionsDecl) ->
			Options = OptionsDecl
			;
			Options = [version(na)|OptionsDecl]).
	
	:- public(input_types/1).
	:- info(input_types/1,
		[ comment is 'InputTypes is a sequence of file types accepted as input to the task.',
		  argnames is ['InputTypes']]).
	input_types(InputTypes) :-
		::declaration(Decl),
		Decl =.. [ _ , InputTypes, _, _ ].

	:- public(output_types/1).
	:- info(output_types/1,
		[ comment is 'OutputTypes is a sequence of file types produced by the task.',
		  argnames is ['OutputTypes']]).
	output_types(OutputTypes) :-
		::declaration(Decl),
		Decl =.. [ _ , _, _, OutputTypes ].
:- end_object.

:- object(task(Module,Task,_InputFiles,_Options), extends(module_task(Module,Task))).

	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/09,
		comment is '.',
		parnames is ['Module', 'Task', 'InputFiles', 'Options']
	]).

	:- public(run/1).
	:- info(run/1,[
		comment is 'Runs the task(Module,Task,InputFiles,Options) using an invoker (see invoker/1) if the task is not available on file(s). OutputFiles is a list of names of resulting files.',
		argnames is ['OutputFiles']]).
	
	run(OutputFilesList) :-
		self(Self),
		parameter(1,Module),
		parameter(2,Task),
		parameter(3,InputFiles),
		parameter(4,Options),
		report_unless(module(Module)::available)::error(module_not_available(Module)),
		report_unless(Self::has_declaration)::error(no_task_declaration(Module::Task)),
		report_unless(Self::has_implementation)::error(no_task_implementation(Module::Task)),
		::output_types(OutputTypes),
		% If the output-types are not declared as a list,
		% assume task predicate takes a single file argument
		(list::valid(OutputTypes) ->
			OutputTypesList = OutputTypes,
			list::length(OutputTypesList,N),
			list::length(OutputFilesList,N),
			OutputFiles=OutputFilesList
			;
			OutputTypesList = [OutputTypes],
			OutputFilesList = [OutputFiles]),
		config::get(file_manager,FileManager),
		::expand_options(Options,ExpandedOptions),
		meta::map([X,Y]>>(file(X)::canonical(Y)),InputFiles,InputFilesCanonical),
		% If the input-types are not declared as a list, 
		% assume that task predicate takes a single file as argument
		::input_types(InputTypes),
		(list::valid(InputTypes) -> 
			InputFilesCanonicalAdapt = InputFilesCanonical
			;
			% Strip the list if it is not declared in the argument
			[InputFilesCanonicalAdapt] = InputFilesCanonical),
		Goal =.. [ Task, InputFilesCanonicalAdapt, ExpandedOptions, OutputFiles ],
		writeln(Goal),
		(FileManager::result_files(Module,Task,InputFilesCanonical,ExpandedOptions,OutputFilesList) ->
			true % The task has allready been run 
			;
			FileManager::result_files_allocate(Module,Task,InputFilesCanonical,ExpandedOptions,OutputFilesList),
			(module(Module)::builtin ->
				Module::Goal
				;
				::invoker(Invoker),
				module(Module)::interface_file(InterfaceFile,_),
				Invoker::run(InterfaceFile,Goal)),
			!,
			FileManager::result_files_commit(Module,Task,InputFilesCanonical,ExpandedOptions)).

	:- public(typecheck/1).
	:- info(typecheck/1,[
		comments is 'Check that supplied types of the input types are valid and unify OutputTypes to the resulting output types.',
		argnames is ['OutputTypes']]).
	
	% FIXME: Typecheck needs adaptation to non-list types
	typecheck(OutputTypes) :-
		parameter(2,Task),
		parameter(3,SuppliedInputTypes),
		parameter(4,Options),
		::declaration(TaskDeclaration),
		TaskDeclaration =.. [ Task, InputTypes, DeclOptions, OutputTypes ],
		::expand_options(Options,DeclOptions,_),
		term::subsumes(InputTypes,SuppliedInputTypes).

	:- public(invoker/1).
	:- info(invoker/1,
		[ comment is 'Determine InvokerObject to use: module may declare invoke_with/1; otherwise use banpipe_config default_invoker',
		  argnames is ['InvokerObject']]).
	invoker(builtin) :-
		parameter(1,Module),
		module(Module)::builtin.

	invoker(generic_invoker) :-
		parameter(1,Module),
		module(Module)::interface_file(_,generic).
		
	invoker(InvokerName) :-
		parameter(1,Module),
		module(Module)::interface_file(InterfaceFile,prolog),
		prolog_file(InterfaceFile)::member(TaskMatcher),
		TaskMatcher =.. [ ':-', invoke_with(PrologName) ],
		atom_concat(PrologName,'_invoker',InvokerName),
		!.

	invoker(Invoker) :-
		config::get(default_invoker,Invoker).
		
	% TODO: Eventually, I might move guard invokation to run/1 (rather than in script interpreter)
	% to allow unifying variables from the default option settings
:- end_object.
