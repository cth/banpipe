% A paraterized object representing a single file.

:- object(path(_Path)).
% TODO: See what can be moved here...
:- end_object.

:- object(file(Path), extends(path(Path))).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/06,
		comment is 'File wrapper object. The parameter A is a filename atom.']).
	
	:- public(read/1).
	read(Contents) :-
		parameter(1,F),
		open(F,read,Stream),
		writeln(F),
		catch(read_characters(Stream,Contents),_,true),
		close(Stream).

	:- public(write/1).		
	write(Contents) :-
		parameter(1,F),		
		open(F,write,Stream),
		write_characters(Stream,Contents),
		close(Stream).

	:- public(append/1).	
	append(Contents) :-
		parameter(1,F),		
		open(F,append,Stream),
		write_characters(Stream,Contents),
		close(Stream).
	
	:- public(copy_to/1).
	copy_to(TargetFile) :-
		parameter(1,File),
		meta::foldl(atom_concat,'',['cp ',File,' ',TargetFile],ShellCopyCommand),
		shell::exec(ShellCopyCommand).


	:- public(exists/0).
	:- info(exists/0, [ comment is 'True if the file A exists.']).
	:- if((current_logtalk_flag(prolog_dialect, swi))).
		exists :-
			parameter(1,F),
			{exists_file(F)}.
	:- elif((current_logtalk_flag(prolog_dialect, yap))).
		exists :-
			parameter(1,F),
			{file_exists(F)}.
	:- elif((current_logtalk_flag(prolog_dialect, b))).
		exists :-
			parameter(1,F),
			{exists(F)}.
	:- endif.

	:- public(touch/0).
	touch :-
		parameter(1,F),		
		open(F,write,Stream),
		close(Stream).
		
	:- public(delete/0).
% FIXME: I am not sure how portable delete_file is 
%	:- if((current_logtalk_flag(prolog_dialect, b))).
	delete :-
		parameter(1,F),
		{delete_file(F)}.
%	:- endif.
		
	:- public(dirname/1).
	:- info(dirname/1,
		[comment is 'Separate out the directory part (Directory) of a filename',
		 argnames is ['Directory']]).

	% FIXME: This is unix only and somewhat fragile. Write a better and portable version.
	dirname(Directory) :-
		parameter(1,Filename),
		% everything before last '/'=47 is dirname:
		atom_codes(Filename, CharCodes),
		list::append(DirPartCodes, FilePartCodes, CharCodes),
		list::append(_,[47],DirPartCodes), % DirPart should end with a '/'
		\+ list::member(47,FilePartCodes),
		atom_codes(Directory,DirPartCodes).
		
	:- private(read_characters/2).
	read_characters(Stream,Contents) :-
		get_code(Stream,Code),
		((Code == -1) ->
			Contents = []
			;
			Contents = [Code|Rest],
			read_characters(Stream,Rest)).

	:- private(write_characters/2).
	write_characters(_,[]).
	write_characters(Stream,[X|Xs]) :-
		put_code(Stream,X),
		write_characters(Stream,Xs).
:- end_object.

:- object(prolog_file(F), extends(file(F))).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/06,
		comment is 'File wrapper object for Prolog files. The parameter A is a filename atom.'
	]).

	:- public(read_terms/1).
	:- info(read_terms/1,[comment is 'reads Terms from file A', argnames is ['Terms'] ]).
	read_terms(Terms) :-
		parameter(1,F),
		open(F,read,Stream),
		stream_read_terms(Stream,Terms),
		close(Stream).
	
	:- public(write_terms/1).
	:- info(write_terms/1,[	comment is 'writes Terms to file A', argnames is ['Terms'] ]).
	write_terms(Terms) :-
		parameter(1,F),
		open(F,write,Stream),
		stream_write_terms(Stream,Terms),
		close(Stream).
	
	:- public(member/1).
	:- info(member/1,[comment is 'Term is a member of file A', argnames is ['Term']]).
	member(Term) :-
		read_terms(Terms),
		!,
		list::member(Term,Terms).

	:- public(append/1).
	:- info(append/1,[comment is 'Appends Terms to file A',argnames is ['Terms']]).
	append(Terms) :-
		parameter(1,F),
		open(F,append,Stream),
		stream_write_terms(Stream,Terms),
		close(Stream).

	:- public(select/2).
	:- info(select/2,[comment is 'Term is a term from file A and Rest all other Terms in file A',argnames is ['Term','Rest']]).
	select(Term,Rest) :-
		read_terms(FileTerms),
		!,
		list::select(Term,FileTerms,Rest).

	:- private(stream_read_terms/2).
	stream_read_terms(Stream,Terms) :-
		read(Stream,Term),
		((Term == end_of_file) ->
			Terms = []
			;
			Terms = [Term|Rest],
			stream_read_terms(Stream,Rest)).
			
	:- private(stream_write_terms/2).
	stream_write_terms(_,[]).
	stream_write_terms(Stream,[Term|Rest]) :-
		writeq(Stream,Term),
		write(Stream,'.\n'),
		stream_write_terms(Stream,Rest).
:- end_object.

:- object(directory(Path), extends(path(Path))).
	:- public(create/0).
	:- info(create/0, [comment is 'Creates the directory if it does not allready exist']).
	
	create :-
		parameter(1,Path),
		((::exists) ->
			true
			;
			shell::make_directory(Path)).

	:- public(exists/0).
	:- info(exists/0,[comment is 'True if the directory exists']).
    	% FIXME: This is not going to work for all Prologs
	exists :- 
	    	parameter(1,Path),
	    	file(Path)::exists.

:- end_object.
