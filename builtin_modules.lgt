:- protocol(banpipe_builtin_module).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/17,
		comment is 'Built-in modules are Logtalk objects which are like banpipe modules (they contain tasks) but they are executed within the main process.']).
		
	:- public(task/1).
	:- info(task/1, [
		comment is 'Task is unified with task declarations of the tasks implemented by the built-in module',
		argnames is ['Task']]).
:- end_protocol.

:- object(file, implements(banpipe_builtin_module)).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/17,
		comment is 'Built-in module "file" ']).
	
	% Task declaration: get/3.
	task(get([_],[filetype(T)],[T])).
	
	:- public(get/3).
	:- info(get/3, [
		comment is 'Implementation of the get task',
		argnames is ['Inputfiles','Options','Outputfiles']]).

	% Getting via http or ftp
	get([TargetURL],_Options,[OutputFile]) :-
		atom_codes(TargetURL,TargetSyms),
		meta::map([X,Y]>>atom_codes(X,Y), ['ftp://','http://', '"ftp://', '"http://'],Matchers),
		list::member(MatchSyms,Matchers),
		list::append(MatchSyms,_,TargetSyms),
		::wget(TargetURL,OutputFile).

	% Getting a file from the local file system
	get([FileURL],Options,[OutputFile]) :-
		atom(File),
		atom_codes(FileURL,FileURLSyms),
		atom_codes('file://', MatchSyms),
		list::append(MatchSyms,FileCodes,FileURLSyms),
		atom_codes(File,FileCodes),
		::get([File],Options,[OutputFile]).

	get([File],_Options,[OutputFile]) :-
		file(File)::exists(File),
		file(File)::copy_to(OutputFile).

	% utility predicate for fetching a file via http	
	wget(URL,OutputFile) :-
		meta::foldl(atom_concat,'',[wget, ' "', URL, '" --output-document=', OutputFile],Command),
		shell::exec(Command).
:- end_object.