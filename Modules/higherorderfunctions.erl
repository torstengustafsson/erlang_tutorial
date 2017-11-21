-module(higherorderfunctions).

-compile(export_all).

one() -> 1.
two() -> 2.
add(X,Y) -> X() + Y().

% Note: the two functions 'increment' and
% 'decrement' below are very similar
increment([])    -> [];
increment([H|T]) -> [H+1|increment(T)].
decrement([])    -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% The function 'map' takes a function and applies
% it on  each element in the list
% example usage: map(incr/1, [1,2,3]). = [2,3,4]
% or, using an anonymous function:
%    map(fun(X) X*2 end, [1,2,3]). = [2,4,6]
incr(X) -> X + 1.
decr(X) -> X - 1.
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

% Used as:
%   MyAlarm = set_alarm("bathroom"),    % set alarm
%   MyAlarm().                          % trigger alarm
set_alarm(Room) ->
  PrepareAlarm = fun(AlarmRoom) ->
    io:format("Alarm set in ~s.~n",[AlarmRoom]),
    fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[AlarmRoom]) end
  end,
  PrepareAlarm(Room).

set_looping_alarm(Room) ->
  PrepareAlarm = fun(AlarmRoom) ->
    io:format("Alarm set in ~s.~n",[AlarmRoom]),
    Loop = fun Looping(N) ->
      io:format("Alarm tripped in ~s! Call Batman!~n",[AlarmRoom]),
      timer:sleep(500), % run alarm once every 0.5 seconds
      if N > 1 -> Looping(N-1);
         N =< 1  -> 'nananananananana BATMAN!'
      end         % for N times.
    end,
    fun() -> Loop(10) end
  end,
  PrepareAlarm(Room).

% local variables can be used inside anonymous
% functions, like this
scope(A) ->
  B = A + 1,
  F = fun() -> A * B end,
  F().

% filter a list based on a predicate function
% ex. only keep even numbers:
% filter(fun(X) -> X rem 2 == 0 end, [1,2,3,4,5]). = [2,4]
%
filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.

% fold a list (only keep one value) based on a predicate function.
% ex. get maximum value:
% [H|T] = [1,6,3,7,4,2]
% fold(fun(X, Y) when X > Y -> X; (_,Y) -> Y end, H, T). = 7
%
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).


% fold can be used to build a list as well, and can be used
% by functions map and filter.
% To build a list, make predicate function return a list (in the form [H|T]) on true.
% map2/2 and filter2/2 are used exactly like map/2 and filter/2.
reverse(L) ->
  fold(fun(X,Acc) -> [X|Acc] end, [], L).

map2(F,L) ->
  reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred, L) ->
   F = fun(X,Acc) ->
        case Pred(X) of
          true  -> [X|Acc];
          false -> Acc
        end
      end,
  reverse(fold(F, [], L)).
