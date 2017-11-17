-module(functions).

% Just export all functions instead of having to export them one-by-one
% -compile(export_all).

-export([greet/2,
         head/1,
         tail/1,
         second/1,
         same/2,
         not_same/2,
         get_element/2,
         valid_time/1,
         temp_convert/1]).


greet(male, Name) ->
  io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
  io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
  io:format("Hello, ~s!", [Name]).

% Return head of a list. Copy of elang:hd/1
head([H|_]) -> H.

% Return tail of a list
tail([_|T]) -> T.

% Return second value of a list
second([_,X|_]) -> X.

% Check equivalency
same(X,X) -> true;
same(_,_) -> false.

not_same(X,X) -> false;
not_same(_,_) -> true.


% example of a recursive function (used from get_element/2)
get_element(List, Index, Current) ->
  if
    (Index == Current) -> head(List);
    true -> get_element(tail(List), Index, Current+1)
  end.

% this method uses the recursive function
get_element(List, Index) ->
  get_element(List, Index, 0).


valid_time({{Y,M,D}, {H,Min,S}}) ->
  io:format("The date is: ~p/~p/~p,~n", [Y,M,D]),
  io:format("The time is: ~p:~p:~p,~n", [H,Min,S]);
valid_time(_) ->
  io:format("not a valid time! Write in the format valid_time({{17,11,06}, {16,25,30}})\n").

temp_convert({Type, Temp}) ->
  case Type of
    celsius   -> {farenheit, 9/5*Temp+32};
    farenheit -> {celsius,   5/9*(Temp-32)};
    true -> unknown
  end.
