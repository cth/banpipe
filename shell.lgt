:- object(shell).

	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/13,
		comment is 'Interaction with shell/OS. Sending shell commands, environment variables etc.']).

	:- public(environment_variable/2).
	:- info(environment_variable/2,
		[ comment is 'If there is an environment variable with name Key, then Value is the value of the environment variable (atom)',
		argnames is ['Key','Value']]).
	
	:- public(exec/1).
	:- info(exec/1, [comment is 'Send Command to the OS shell', argnames is ['Command']]).
	
	:- public(exec/2).
	:- info(exec/2, [comment is 'Send Command to the OS shell and bind Status to the status returned from the OS', argnames is ['Command', 'Status']]).	
	
	:- if((current_logtalk_flag(prolog_dialect, swi))).
		environment_variable(Key,Var) :-
			{getenv(Key,Var)}.

		exec(ShellCmd) :-
			{shell(ShellCmd)}.
			
		exec(ShellCmd,Status) :-
			{shell(ShellCmd,Status)}.
			
	:- elif((current_logtalk_flag(prolog_dialect, b))).
		environment_variable(Key,Var) :-
			{environ(Key,Var)}.
	
		exec(ShellCmd) :-
			{system(ShellCmd)}.
			
		exec(ShellCmd,Status) :-
			{system(ShellCmd,Status)}.
	:- endif.
	
	:- public(make_directory/1).
	make_directory(Directory) :-
		{make_directory(Directory)}.
		
	:- public(delete_directory/1).
	delete_directory(Directory) :-
		{delete_directory(Directory)}.
	
:- end_object.