:- task(hello([],[language(english)],[text(hello)])).

hello([],Options,[OutFile]) :-
	membr(language(L), Options),
	tell(OutFile),
	((L == english) -> write(hello) ;
	((L == spanish) -> write(hola)) ;
	fail),
	told.

:- task(world([],[language(english)],[text(world)])).
	
world([],Options,[OutFile]) :-
	membr(language(L), Options),
	tell(OutFile),
	((L == english) -> write(world) ;
	((L == spanish) -> write(mundo)) ;
	fail),
	told.
	
:- task(helloworld([text(A),text(B)],[],[text(A+B)])).

helloworld([HelloFile,WorldFile],_,[OutFile]) :-
	% Read hello file
	read_from_file(HelloFile,Hello),
	% Read world file
	read_from_file(WorldFile,World),
	% merge in "hello world" file
	tell(OutFile),
	atom_codes(HelloAtom,Hello),
	write(HelloAtom),
	write(' '),
	atom_codes(WorldAtom,World),
	write(WorldAtom), 
	write('\n'),
	told.

% Helper predicates
	
% member/2.
membr(X,[X|_]).
membr(X,[_|Xs]) :- membr(X,Xs).

read_from_file(File,Contents) :-
	open(File,read,Stream),
	read_from_stream(Stream,Contents),
	close(Stream).

read_from_stream(Stream,Contents) :-
	get_code(Stream,Ch),
	((Ch == end_of_file) ->
		Contents = []
		;
		Contents = [Ch|Rest],
		read_from_stream(Stream,Rest)).
