-module(exceptions).

-compile(export_all).

% check if function throws exception. If it does return thrown value in a tuple.
% if no error, return ok.
% ex:
% throws(fun() -> throw(thrown) end). = {throw, caught, thrown}
catch_exception(F) ->
  try F() of
    _ -> ok
  catch
    Throw -> {throw, caught, Throw};       % when no type is mentioned, a throw is assumed
    error:Error -> {error, caught, Error}; % catches errors (ends execution of the current process)
    exit:Exit -> {exit, caught, Exit}      % catches exits (kills current process)
  end. % difference of error and exit: erorr keeps a stacktrace, which can be displayed for debugging


% Black Knight debugging.

sword(1) -> throw(slice);          % usage: black_knight(fun() -> sword(1) end). = "Tis but a scratch."
sword(2) -> erlang:error(cut_arm); % usage: black_knight(fun() -> sword(2) end). = "I've had worse."
sword(3) -> exit(cut_leg);         % usage: black_knight(fun() -> sword(3) end). = "Come on you pansy!"
sword(4) -> throw(punch);          % usage: black_knight(fun() -> sword(4) end). = "Just a flesh wound."
sword(5) -> exit(cross_bridge).    % usage: black_knight(fun() -> sword(5) end). = "Just a flesh wound."

% Handles three specific exceptions with special messages,
% and any other with a "Just a flesh wound." message.
% A function without exceptions returns the message "None shall pass.".
black_knight(Attack) when is_function(Attack, 0) ->
  try Attack() of
    _ -> "None shall pass."
  catch
    throw:slice -> "Tis but a scratch.";
    error:cut_arm -> "I've had worse.";
    exit:cut_leg -> "Come on you pansy!";
    _:_ -> "Just a flesh wound."
  end.

% Multiple expressions can be used inside the try-catch block.
% This will for example always return {caught,throw,up}
% Note that we do not have a 'of' that handles any return values here.
multi_throw() ->
  try ok, fine, throw(up) catch
    Exception:Reason -> {caught, Exception, Reason}
  end.

% Note: anything between 'try' and 'of' is in a protected area, meaning a reference to
% the original function must be kept. This means tail-recursion is not possible here.
% A try-catch without an 'of' part is also in a protected area.


