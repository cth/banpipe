:- object(uri(_URI)).

	:- info([
		version is 1:0:0,
		author is 'Christian Theil Have',
		date is 2012-12-06,
		comment is 'Object to represent URLs',
		parnames is ['URI']
	]).

	:- private(internet_protocol/1).
	internet_protocol('https://').
	internet_protocol('http://').
	internet_protocol('ftp://').

	:- private(local_protocol/1).
	local_protocol('file://').

	:- private(protocol/1).
	protocol(P) :-
		::internet_protocol(P).
	protocol(P) :-
		::local_protocol(P).

	:- public(valid/0).
	:- info(valid/0,[comment is 'True if URI represented by object is valid.']).
	valid :- 
		::elements(_,_).

	:- public(is_url/0).
	:- info(is_url/0,[comment is 'True if the file name begins with an URL identifier (e.g. http://...)']).
	is_url :-
		::internet_protocol(Protocol),
		::elements(Protocol,_).

	:- public(elements/2).
	:- info(elements/2, [
		comment is 'For, e.g., http://banpipe.org/index.html, Protocol is \'http://\' and Filepart is \'banpipe.org/index.html\'',
		argnames is ['Protocol','Filepart']
	]).

	elements(Protocol,Filepart) :-
		parameter(1,URI),
		atom_codes(URI,URICodes),
		::protocol(Protocol),
		atom_codes(Protocol,ProtocolCodes),
		list::append(ProtocolCodes,FilepartCodes,URICodes),
		atom_codes(Filepart,FilepartCodes).

:- end_object.