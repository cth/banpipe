:- object(reporting).

	:- public(error/1).
	:- public(warning/1).
	:- public(info/1).

	:- protected(check_condition/0).

	error(Msg) :-
		::check_condition,
		!,
		write('ERROR: '),
		write(Msg), nl,
		throw(Msg).
	error(_).

	warning(Msg) :-
		::check_condition,
		!,
		write('WARNING: '),
		write(Msg), nl.
	warning(_).

	info(Msg) :-
		::check_condition,
		!,
		write('INFO: '),
		write(Msg), nl.
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