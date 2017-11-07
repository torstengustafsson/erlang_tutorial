-module(guards).

% Just export all functions instead of having to export them one-by-one
-compile(export_all).

% check if a person is a teenager using a guard
is_teenager(X) when X >= 13, X =< 19 ->
  true;
is_teenager(_) ->
  false.

% an if must always return something (we need a true branch)
oh_god(N) ->
  if N =:= 2 -> might_succeed;
     true    -> always_does
  end.

what_does_it_say(Animal) ->
  Talk =  if Animal == cat  -> "meow";
             Animal == beef -> "mooo";
             Animal == dog  -> "bark";
             Animal == tree -> "bark";
             true -> "fgdadfgna"
         end,
  atom_to_list(Animal) ++ " says " ++ Talk ++ "!".

% insert X to  a list if it is not already in it, using a case clause
insert(X, []) ->
  [X];
insert(X, Set) ->
  case lists:member(X, Set) of
    true  -> Set;
    false -> [X|Set]
  end.
% this would do the same thing:
%   if lists:member(X, Set) -> Set;
%      true                 -> [X|Set]
%   end.

% case clause allows us to use guards for each case, which we can not do with if
% a type guard (type test BIF) is used before entering the function to ensure 'Temperature' is a tuple.
beach(Temperature) when is_tuple(Temperature), tuple_size(Temperature) =:= 2 ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
      'favorable';
    {kelvin, N} when N >= 293, N =< 318 ->
      'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 ->
      'favorable in the US';
    _ ->
    'avoid beach'
  end;
beach(_) ->
  io:format("Bad argument! Use in the form: beach({celsius, 25.0})\n").

% this would do the same thing:
% beach({celsius, N}) when N >= 20, N =< 45 ->
%   'favorable';
% beach({kelvin, N}) when N >= 293, N =< 318 ->
%   'scientifically favorable';
% beach({fahrenhet, N}) when N >= 68, N =< 113 ->
%   'favorable in the US';
% beach(_) ->
%   'avoid beach'.

% Which is best? Function calls or 'case ... of' clauses?
% Both are very much the same ad it doesn't really seem to matter.
% One difference is that function calls can easier pattern match agains
% individual values, but case clauses are considered cleaner when you can use them

% Note: if:s is not really needed when we have 'case ... of'. It is there as a short way to
%       use guards without having to write the whole pattern matching part when you don't need it


