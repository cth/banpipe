:- protocol(banpipe_interpreter).
	:- info([
		version is 1.0,
		author is 'Christian Theil Have',
		date is 2012/11/16,
		comment is 'This protocol represents an banpipe interpreter. Actual concrete interpreter (which may have various semantics) implement this protocol.']).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the banpipe interpreter for Goal',
		argnames is ['Goal']]).		
:- end_protocol.
