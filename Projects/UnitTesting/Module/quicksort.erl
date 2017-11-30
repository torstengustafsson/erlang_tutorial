%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quicksort Algorithm.
%%
%% This module was taken as an example for eUnit
%% testing. Some tests is defined to ensure the
%% sorting works as expected.
%%
%% From the shell, compile the module with:
%%
%%    c(quicksort).
%%
%% the unit tests can then be run by running:
%%
%%    quicksort:test().
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(quicksort).

% include eUnit
-include_lib("eunit/include/eunit.hrl").

-export([quicksort/1,
         lc_quicksort/1]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  {Smaller, Larger} = partition(Pivot, Rest, [], []),
  quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger);
quicksort(_) ->
 {error, caught, "Error: A list was not proveded as argument!"}.

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
  if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
     H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
  end.

% quicksort with list comprehensions
% easier to read, but has to traverse the list twice
% to partition the list in smaller and larger parts
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
  lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
  ++ [Pivot] ++
  lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).


% define some unit tests.
% Note: all functions with names ending in '_test()' is automatically
%       exported as unit tests by eUnit.

already_sorted_test() ->
  [1,2,3] = quicksort([1,2,3]).

length_test() ->
  ?assert(length(quicksort([1,2,3])) =:= 3).

sort_test() ->
  [1,2,3,4,5] = quicksort([1,3,5,2,4]).

empty_list_test() ->
  quicksort([]).

badarg_test() ->
  quicksort(not_a_list).
