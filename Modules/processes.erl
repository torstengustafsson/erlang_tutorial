
-module(processes).

-export([double/1, double_process/0, wait/0, wait_process/0]).

-define(TIMEOUT, 1000).

% process that return an integer doubled
double(Val) ->
  io:format("doule: creating process...~n"),
  Pid = spawn(?MODULE, double_process, []), % create process
  Pid ! {self(), Val},                      % send message to process
  receive                                   % wait for response from process
    {SenderId, Value} ->
      io:format("double: result ~p received from process id ~p!~n", [Value, SenderId]),
      Value
    after ?TIMEOUT -> error(timeout)
  end.

double_process() ->
  io:format("double_process: process spawned!~n"),
  receive
    {SenderPid, Value} when is_integer(Value) ->
      io:format("double_process: value ~p received from process id ~p!~n", [Value, SenderPid]),
      SenderPid ! {self(), Value * 2};
%     Value * 2; % This seems to return to the process that created it, i.e. the same thing in this case
    _          -> error(invalid_format)
    after ?TIMEOUT -> error(timeout)
  end.


% process that waits for timeout
wait() ->
  spawn(?MODULE, wait_process, []).
wait_process() ->
  receive
    after ?TIMEOUT -> ok
  end.
