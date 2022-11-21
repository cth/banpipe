
:- object(fix_directives,
	implements(expanding)).

	term_expansion((:- invoke_with(System)), (:- initialization(invoke_with(System)))).
	term_expansion((:- task(Task)), (:- initialization(task(Task)))).

:- end_object.
