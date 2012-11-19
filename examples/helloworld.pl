% A simple banpipe script to implement the classic "Hello World"
% This simple example, however, illustrates several banpipe script concepts.
% It contains three dependency rules (the latter two of which have guards) and 
% all of which are parameterized rules (i.e. the L variable). It also illustrates
% how the rules may interact with regular prolog rules (valid_language/1) through the guard. 
% 
% Author: Christian Theil Have, 2012.

helloworld(L) <- helloworld::helloworld([hello(L),world(L)],[]).

hello(L) <- valid_language(L) | helloworld::hello([],[language(L)]).

world(L) <- valid_language(L) | helloworld::world([],[language(L)]).

% Ordinary prolog facts (used in guards)
valid_language(english).
valid_language(spanish).
