%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Printer Module.
%%
%% This module was taken as an example for eUnit
%% testing. Unit tests is defined in
%% 'printer_tests.erl'.
%%
%% From the shell, compile the modules with:
%%
%%    c(printer).
%%    c(printer_tests).
%%
%% the unit tests can then be run by running:
%%
%%    printer:test().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(printer).

% include eUnit
-include_lib("eunit/include/eunit.hrl").

-export([print_cool/0,
         print_cool_string/0]).

print_cool() ->
  io:format("cool\n"),
  ok.

print_cool_string() ->
  "cool".