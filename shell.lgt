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
	:- if((current_logtalk_flag(prolog_dialect, swi))).
		environment_variable(Key,Var) :-
			{getenv(Key,Var)}.
	:- else.
		environment_variable(Key,Var) :-
			{environ(Key,X)}, % If yap is called with X='', then it succeeds (counterintuitively)
			Var = X.
    	:- endif.
		
	:- public(working_directory/1).
	:- info(working_directory/1,
		[ comment is 'Bind Dir to the current working directory',
		  argnames is ['Dir']]).
	:- if((current_logtalk_flag(prolog_dialect, b))).
		working_directory(Dir) :-
			{get_cwd(Dir)}.
	:- else.
		working_directory(Dir) :-
			{working_directory(Dir,Dir)}.
    	:- endif.

	:- public(change_directory/1).
	:- info(change_directory/1,
		[ comment is 'Change the current working directory to NewDir',
		  argnames is ['NewDir']]).
	:- if((current_logtalk_flag(prolog_dialect, b))).
		change_directory(NewDir) :-
			{chdir(NewDir)}.
	:- else.
		change_directory(NewDir) :-
			{working_directory(_,NewDir)}.
    	:- endif.
		
	:- public(exec/1).
	:- info(exec/1, [comment is 'Send Command to the OS shell', argnames is ['Command']]).
    	exec(Command) :-
	    shell::exec(Command,_).
	
	:- public(exec/2).
	:- info(exec/2, [comment is 'Send Command to the OS shell and bind Status to the status returned from the OS', argnames is ['Command', 'Status']]).	
	:- if((current_logtalk_flag(prolog_dialect, swi))).
		exec(ShellCmd,Status) :-
			{shell(ShellCmd,Status)}.
	:- else.
		exec(ShellCmd,Status) :-
			{system(ShellCmd,Status)}.
    	:- endif.
	
	:- public(make_directory/1).
	make_directory(Directory) :-
		{make_directory(Directory)}.
		
	:- public(delete_directory/1).
	:- if((current_logtalk_flag(prolog_dialect,yap))).
	delete_directory(Directory) :-
    		{delete_file(Directory,[directory])}.
   	:- else.
	delete_directory(Directory) :-
		{delete_directory(Directory)}.
	:- endif.
		
:- end_object.
