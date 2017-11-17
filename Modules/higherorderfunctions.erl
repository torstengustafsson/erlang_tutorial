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
filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.
