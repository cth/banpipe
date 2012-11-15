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
		
	:- public(working_directory/1).
	:- info(working_directory/1,
		[ comment is 'Bind Dir to the current working directory',
		  argnames is ['Dir']]).
		
	:- public(change_directory/1).
	:- info(change_directory/1,
		[ comment is 'Change the current working directory to NewDir',
		  argnames is ['NewDir']]).
		
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
			
		working_directory(Dir) :-
			{working_directory(Dir,Dir)}.
			
		change_directory(NewDir) :-
			{working_directory(_,NewDir)}.
	:- elif((current_logtalk_flag(prolog_dialect, b))).
		environment_variable(Key,Var) :-
			{environ(Key,Var)}.
	
		exec(ShellCmd) :-
			{system(ShellCmd)}.
			
		exec(ShellCmd,Status) :-
			{system(ShellCmd,Status)}.
			
		working_directory(Dir) :-
			{get_cwd(Dir)}.
			
		change_directory(NewDir) :-
			{chdir(NewDir)}.
	:- endif.
	
	:- public(make_directory/1).
	make_directory(Directory) :-
		{make_directory(Directory)}.
		
	:- public(delete_directory/1).
	delete_directory(Directory) :-
		{delete_directory(Directory)}.
		
	
	
	
:- end_object.