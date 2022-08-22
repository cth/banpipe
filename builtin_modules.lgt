:- protocol(banpipe_builtin_module).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-17,
		comment is 'Built-in modules are Logtalk objects which are like banpipe modules (they contain tasks) but they are executed within the main process.'
	]).

	:- public(task/1).
	:- info(task/1, [
		comment is 'Task is unified with task declarations of the tasks implemented by the built-in module',
		argnames is ['Task']
	]).

:- end_protocol.


:- object(file, implements(banpipe_builtin_module)).
	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-17,
		comment is 'Built-in module "file" '
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% Task declaration: get/3.
	task(get([_],[filetype(T)],[T])).
	
	:- public(get/3).
	:- info(get/3, [
		comment is 'Implementation of the get task',
		argnames is ['Inputfiles','Options','Outputfiles']
	]).

	% Getting via http or ftp
	get([TargetURL],_Options,[OutputFile]) :-
		uri(TargetURL)::is_url,
		::wget(TargetURL,OutputFile).

	% Getting a file from the local file system
	get([FileURI],_Options,[OutputFile]) :-
		uri(FileURI)::elements('file://',Filename),
		file(Filename)::canonical(File),
		file(File)::exists,
		file(File)::copy_to(OutputFile).

	:- private(wget/2).
	:- info(wget/2, [
		comment is 'fetching a file via http/ftp using wget utility',
		argnames is ['URL','OutputFile']
	]).

	wget(URL,OutputFile) :-
		atomic_list_concat([wget, ' "', URL, '" --output-document=', OutputFile],Command),
		os::shell(Command).

:- end_object.


:- object(aggregate, implements(banpipe_builtin_module)).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2013-02-11,
		comment is 'Aggregate a series of files.'
	]).

	:- uses(integer, [
		between/3
	]).

	% This is defined to work for all with upto 100 input files
	task(depend_all(L,[],[pseudo])) :-
		between(2,100,N),	
		list::length(L,N).

	:- public(depend_all/3).
	:- info(depend_all/3, [
		comment is 'Create a (empty) file to represent aggregation relation of all input files.',
		argnames is ['Inputfiles','Options','Outputfiles']
	]).

	depend_all(_InputFiles,_Options,[OutputFile]) :-
		file(OutputFile)::touch.

:- end_object.
