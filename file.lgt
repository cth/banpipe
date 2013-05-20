% A paraterized object representing a single file.

:- object(path(_Path)).
% TODO: See what can be moved here...
:- end_object.

:- object(file(Path), extends(path(Path))).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/06,
		comment is 'File wrapper object. The parameter A is a filename atom.',
		parnames is ['Path']
	]).

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

	:- public(size/1).
	:- if(current_logtalk_flag(prolog_dialect, swi)).
	size(Size) :-
		parameter(1,F),
		{size_file(F, Size)}.
	:- elif(current_logtalk_flag(prolog_dialect, yap)).
	size(Size) :-
		parameter(1,F),
		{file_property(F,size(Size))}.
	:- elif(current_logtalk_flag(prolog_dialect,b)).
	size(Size) :-
		parameter(1,F),
		{file_property(F,size(Size))}.
	:- endif.

	:- public(exists/0).
	:- info(exists/0, [ comment is 'True if the file (or directory) A exists.']).
	:- if((current_logtalk_flag(prolog_dialect, swi))).
		exists :-
			parameter(1,F),
			{exists_file(F) ; exists_directory(F) }.
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
        delete :-
                parameter(1,F),
                catch({delete_file(F)}, _, true).
		
	:- public(dirname/1).
	:- info(dirname/1,
		[comment is 'Separate out the directory part (Directory) of a filename',
		 argnames is ['Directory']]).

	dirname(Directory) :-
	    ::dir_file(Directory,_).

	:- public(basename/1).
    	basename(Filename) :-
	    ::dir_file(_,Filename).

	:- private(dir_file/2).
    	:- info(dir_file/2,[
		comment is 'Separates filename into a Directory part and a Filename part',
		argnames is ['Directory','Filename']]).
	% FIXME: This is unix only and somewhat fragile. Write a better and portable version.
	dir_file(Directory,Filepart) :-
		parameter(1,Filename),
		% everything before last '/'=47 is dirname:
		atom_codes(Filename, CharCodes),
		list::append(DirPartCodes, FilePartCodes, CharCodes),
		list::append(_,[47],DirPartCodes), % DirPart should end with a '/'
		\+ list::member(47,FilePartCodes),
		atom_codes(Directory,DirPartCodes),
		atom_codes(Filepart,FilePartCodes).

	:- public(canonical/1).
    	:- info(canonical/1, [
		comment is 'CanonicalFilename is Filename with backspaces->slashes and double slashes removed',
		argnames is ['CanonicalFilename']]).
		
	canonical(CanonicalFilename) :-
	    	parameter(1,Filename),
		uri(Filename)::valid,
		!,
		uri(Filename)::elements(Protocol,Filepart),
		atom_codes(Filepart,FilePartCodes),
		% Replace backslash (code 92) with forward slash (code 47)
		meta::map([X,Y]>>((X==92) -> Y=47 ; Y=X),FilePartCodes,UnixFilenameCodes),
		remove_double_slashes(UnixFilenameCodes,Unslashed),
		atom_codes(CanonicalFilepart,Unslashed),
		atom_concat(Protocol,CanonicalFilepart,CanonicalFilename).
		
	canonical(CanonicalFilename) :-
	    	parameter(1,Filename),
		atom_codes(Filename,FileCodes),
		% Replace backslash (code 92) with forward slash (code 47)
		meta::map([X,Y]>>((X==92) -> Y=47 ; Y=X),FileCodes,UnixFilenameCodes),
		remove_double_slashes(UnixFilenameCodes,Unslashed),
		atom_codes(CanonicalFilename,Unslashed).
	
	:- private(remove_double_slashes/2).
    	:- info(remove_double_slashes/2,[
		comment is 'replace adjacent slashes with one slash',
		argnames is ['FilenameIn','FilenameOut']]).
	remove_double_slashes([],[]).
	remove_double_slashes([47,47|FilenameCodesIn],[47|FilenameCodesOut]) :-
	    	!,
	    	remove_double_slashes(FilenameCodesIn,FilenameCodesOut).
	remove_double_slashes([X|FilenameCodesIn],[X|FilenameCodesOut]) :-
	    	remove_double_slashes(FilenameCodesIn,FilenameCodesOut).

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
		comment is 'File wrapper object for Prolog files. The parameter A is a filename atom.',
		parnames is ['File']
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

	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/06,
		comment is '.',
		parnames is ['Path']
	]).

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
