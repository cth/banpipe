:- object(reporting).
	:- public(error/1).
	:- public(warning/1).
	:- public(info/1).
	
	error(Msg) :-
		check_condition,
		!,
		write('ERROR: '),
		writeln(Msg),
		throw(Msg).

	warning(Msg) :-
		check_condition,
		!,
		write('WARNING: '),
		writeln(Msg).
	
	info(Msg) :-
		check_condition,
		!,
		write('INFO: '),
		writeln(Msg).

	check_condition.
:- end_object.

:- object(report_if(_Cond), extends(reporting)).
	check_condition :-
		parameter(1,Goal),
		call(Goal).
:- end_object.

:- object(report_unless(_Cond), extends(reporting)).
	check_condition :-
		parameter(1,Goal),
		\+ call(Goal).

:- end_object.