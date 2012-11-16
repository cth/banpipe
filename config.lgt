:- object(config).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/15,
		comment is 'Configuration of banpipe.']).
		
	:- private(directive/2).
	:- dynamic(directive/2).
	
	:- initialization(::setup_defaults).

	:- public(push/2).
	:- info(push/2, [
		comment is 'Add configuration with Key/Value - overrides previous entries for Key.',
		argnames is ['Key','Value']]).
	push(Key,Value) :-
		::asserta(directive(Key,Value)).
		
	:- public(pop/2).
	:- info(pop/2, [
		comment is 'Remove configuration directive with Key/Value, resorting to previous definition for Key (if there is one).',
		argnames is ['Key','Value']]).
	pop(Key,Value) :-
		::retract(directive(Key,Value)).
		
	:- public(pop/1).
	:- info(pop/1, [
		comment is 'alias for as pop(Key,_)',
		argnames is ['Key']]).
	pop(Key,Value) :-
		::retract(directive(Key,Value)).
	
	:- public(get/2).
	:- info(get/2, [
		comment is 'Get the current Value for Key',
		argnames is ['Key','Value']]).
	get(Key,Value) :-
		::directive(Key,Value),
		!.
		
	:- public(setup_defaults/0).
	:- mode(setup_defaults, one).
	:- info(setup_defaults/0, [
		comment is 'Sets up default values for necessary configuration directives']).
	setup_defaults :-
		::push(execution_mode,sequential),
		::push(default_invoker,prism_invoker),
		::push(invoke_command(prism),prism),
		shell::working_directory(WorkingDir),
		atom_concat(WorkingDir,'/result_files',ResultFileDir),
		::push(result_file_directory,ResultFileDir),
		atom_concat(ResultFileDir,'/result_file_index',IndexFile),
		::push(index_file,IndexFile),
		::push(file_manager,term_file_index(IndexFile)).
:- end_object.
