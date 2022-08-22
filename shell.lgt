:- object(shell).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-11-13,
		comment is 'Interaction with shell/OS. Sending shell commands, environment variables etc.']).

	:- public(delete_directory/1).
	:- if((current_logtalk_flag(prolog_dialect,yap))).
	delete_directory(Directory) :-
		{delete_file(Directory,[directory])}.
	:- else.
	delete_directory(Directory) :-
		{delete_directory(Directory)}.
	:- endif.

:- end_object.
