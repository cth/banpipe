:- object(events).
	:- private(event/1).
	:- dynamic(event/1).
	:- private(handler/2).
	:- dynamic(handler/2).

	:- public(add_listener/2).
	add_handler(EventPattern, Callback) :-
		::asserta(handler(EventPattern,Callback)).

	:- public(add_event/1).

	% The event allready exists (and have been handled)
	add_event(Event) :-
		events::event(Event),
		writeln(existing_event(Event)),
		!.

	% This is a new event that should be handled appropriately
	add_event(Event) :-
		::asserta(event(Event)),
		writeln(new_event(Event)),
		forall(handler(Event,Handler), call(Handler)).
:- end_object.
