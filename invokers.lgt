:- protocol(invokerp).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Protocal that invokers must implement']).
		
	:- public(run/2).
	:- info(run/2, [ 
		comment is 'Run Goal within the file InterfaceFile',
		argnames is ['InterfaceFile','Goal']]).
		
:- end_protocol.

:- object(logger_invoker,implements(invokerp)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Simple invoker which does nothing, but logs the invocation.'
	]).
		
	run(InterfaceFile,Goal) :-
		write('(simulating) Running goal '),
		write(Goal), nl,
		write(' in file '), nl,
		write(InterfaceFile), nl.
:- end_object.

:- object(generic_invoker, implements(invokerp)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2013-01-30,
		comment is 'Invoker which launces a program or script as specified by a generic interface file'
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	run(InterfaceFile,Goal) :-
		os::working_directory(CurrentDir),
		file(InterfaceFile)::dirname(ModuleDir),
		os::change_directory(ModuleDir),
		Goal =.. [ Task, InputFiles, Options, OutputFiles ],
		prolog_file(InterfaceFile)::read_terms(Terms),
		list::member(invoke(Task,BaseCommand), Terms),
		meta::map([Opt,OptStr]>>(generic_invoker::option_str(Opt,OptStr)), Options,OptStrings),
		list_extras::intersperse(' ', OptStrings,OptStringsSep),
		list_extras::intersperse(' ', InputFiles, InputFilesSep),
		list_extras::intersperse(' ', OutputFiles, OutputFilesSep),
		meta::foldl(list::append,[],[[BaseCommand],[' '], InputFilesSep, [' '], OutputFilesSep, [' '], OptStringsSep],CommandList),
		meta::foldl(atom_concat,'', CommandList, Command),
		write(shell_command(Command)), nl,
		os::shell(Command),
		os::change_directory(CurrentDir).


	:- private(option_str/2).
	option_str(Option, OptionStr) :-
		Option =.. [ Key, Value ],
		atomic_list_concat(['--',Key,' ',Value],OptionStr).

:- end_object.

:- object(prolog_invoker, implements(invokerp)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is '(abstract) Invoker which launches a Prolog process and runs the goal within that process'
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	run(InterfaceFile,Goal) :-
		os::working_directory(CurrentDir),
		file(InterfaceFile)::dirname(ModuleDir),
		file(InterfaceFile)::basename(BaseFileName),
		os::change_directory(ModuleDir),
		::key_invoke_command(InvokeCmdKey),
		(config::get(InvokeCmdKey,Exec) ->
			true
			;
			::default_invoke_command(Exec)),
		term_extras::term_to_atom(Goal,GoalAtom),
		::goal_option(GoalOption),
		atomic_list_concat([Exec,' ', GoalOption, ' "assertz(task(_)), assertz(invoke_with(_)), consult(\'', BaseFileName, '\'),', GoalAtom,',halt."'],Command),
		write(shell_command(Command)), nl,
		os::shell(Command),
		os::change_directory(CurrentDir).

	:- protected(key_invoke_command/1).
	key_invoke_command(invoke_command(prolog)).
	
	:- protected(default_invoke_command/1).
	default_invoke_command(prolog).
	
	:- protected(goal_option/1).
	:- info(goal_option/1, [
		comment is 'Option is an atom which specifies a command line option which is used to indicated to the prolog process that the following entity is a string representing a goal to be executed.',
		argnames is ['Option']]).
	goal_option('-g').
	
:- end_object.

:- object(bp_invoker,extends(prolog_invoker)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Invoker which launches a B-Prolog process and runs the goal within that process']).
		
	key_invoke_command(invoke_command(bp)).
	default_invoke_command(bp).
	goal_option('-g').
:- end_object.

:- object(prism_invoker,extends(prolog_invoker)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Invoker which launches a PRISM process and runs the goal within that process']).
		
	key_invoke_command(invoke_command(prism)).
	default_invoke_command(prism).
	goal_option('-g').
:- end_object.


:- object(swipl_invoker, extends(prolog_invoker)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Invoker which launches a SWI-Prolog process and runs the goal within that process']).
	
	key_invoke_command(invoke_command(swipl)).
	default_invoke_command(swipl).
	goal_option('-g').
:- end_object.


:- object(yap_invoker, extends(prolog_invoker)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Invoker which launches a YAP process and runs the goal within that process']).
	
	key_invoke_command(invoke_command(yap)).
	default_invoke_command(yap).
	goal_option('-g').
:- end_object.

:- object(gprolog_invoker, extends(prolog_invoker)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-14,
		comment is 'Invoker which launches a GNU-Prolog process and runs the goal within that process']).
		
	key_invoke_command(invoke_command(gprolog)).
	default_invoke_command(gprolog).
	goal_option('--entry-goal').
:- end_object.
