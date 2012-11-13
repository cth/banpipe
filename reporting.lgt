:- object(reporting).
	:- public(error/1).
	:- public(warning/1).
	
	error(Error) :-
		check_condition,
		!,
		writeln(throwing),
		throw(Error).

	warning(Warning) :-
		check_condition,
		!,
		writeln(Warning).

	check_condition.
:- end_object.

:- object(throw_if(_Cond), extends(reporting)).
	check_condition :-
		parameter(1,Goal),
		call(Goal).
:- end_object.

:- object(throw_unless(_Cond), extends(reporting)).
	check_condition :-
		parameter(1,Goal),
		\+ call(Goal).

:- end_object.