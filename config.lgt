:- object(config).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-15,
		comment is 'Configuration of banpipe.'
	]).

	:- private(directive/2).
	:- dynamic(directive/2).

	:- initialization(::setup_defaults).

	:- public(push/2).
	:- info(push/2, [
		comment is 'Add configuration with Key/Value - overrides previous entries for Key.',
		argnames is ['Key','Value']
	]).

	push(Key,Value) :-
		::asserta(directive(Key,Value)).

	:- public(pop/2).
	:- info(pop/2, [
		comment is 'Remove configuration directive with Key/Value, resorting to previous definition for Key (if there is one).',
		argnames is ['Key','Value']
	]).

	pop(Key,Value) :-
		::retract(directive(Key,Value)).

	:- public(pop/1).
	:- info(pop/1, [
		comment is 'alias for as pop(Key,_)',
		argnames is ['Key']
	]).

	pop(Key) :-
		::retract(directive(Key,_)).

	:- public(get/2).
	:- info(get/2, [
		comment is 'Get the current Value for Key',
		argnames is ['Key','Value']
	]).

	get(Key,Value) :-
		::directive(Key,Value),
		!.

	:- public(set/2).
	:- info(set/2,[
		comment is 'Set is current Value for Key. No effect is current value for Key is Value',
		argnames is ['Key','Value']
	]).

	set(Key,Value) :-
		(	::get(Key,Value) ->
			true
		;	::push(Key,Value)
	).

	:- public(set/1).
	:- info(set/2, [
		comment is 'Set is current value for Key to true.',
		argnames is ['Key','Value']
	]).

	set(Key) :-
		::set(Key,true).

	:- public(setup_defaults/0).
	:- mode(setup_defaults, one).
	:- info(setup_defaults/0, [
		comment is 'Sets up default values for necessary configuration directives'
	]).

	setup_defaults :-
		::push(execution_mode,sequential),
		::push(default_invoker,prism_invoker),
		::push(invoke_command(prism),prism),
		os::working_directory(WorkingDir),
		atom_concat(WorkingDir,'/result_files',ResultFileDir),
		::push(result_file_directory,ResultFileDir),
		atom_concat(ResultFileDir,'/result_file_index',IndexFile),
		::push(index_file,IndexFile),
		::push(file_manager,term_file_index(IndexFile)).

:- end_object.
