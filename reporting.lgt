:- object(reporting).
	:- public(error/1).
	:- public(warning/1).
	:- public(info/1).
	
	:- protected(check_condition/0).
	
	error(Msg) :-
		::check_condition,
		!,
		write('ERROR: '),
		writeln(Msg),
		throw(Msg).
	error(_).

	warning(Msg) :-
		::check_condition,
		!,
		write('WARNING: '),
		writeln(Msg).
	warning(_).
	
	info(Msg) :-
		::check_condition,
		!,
		write('INFO: '),
		writeln(Msg).
	info(_).

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
		(call(Goal) -> fail ; true).
:- end_object.