-module(subprocess).

-export([run/1, run/2, run/3, cmdpp/1]).

% Run the specified executable and return ok on success
-spec run(string()) -> ok | {error, non_neg_integer()}.
run(Executable) ->
    run(Executable, []).

% Run an executable with the arguments passed in as a list
-spec run(string(), [string()]) -> ok | {error, non_neg_integer()}.
run(Executable, Args) ->
    run(Executable, Args, <<>>).

% Run an executable with arguments and send Input to it
-spec run(string(), [string()], binary()) -> ok | {error, non_neg_integer()}.
run(Executable, Args, Input) ->
    case os:find_executable(Executable) of
	false ->
	    exit(enoent);
	FoundExecutable ->
	    Port = open_port({spawn_executable, FoundExecutable},
			     [exit_status, {args, Args}, stderr_to_stdout]),
	    Port ! {self(), {command, Input}},
	    loop_till_done(Port)
    end.

-spec loop_till_done(port()) -> ok | {error, non_neg_integer()}.
loop_till_done(Port) ->
    receive
	{Port, {data, _Data}} ->
	    % Throw out anything coming in from stdin
	    loop_till_done(Port);
	{Port, {exit_status, 0}} ->
	    ok;
	{Port, {exit_status, ExitStatus}} ->
	    {error, ExitStatus}
    end.

% Run a command using os:cmd/1, but pretty print its output
% in nice lines instead of one really long string.
-spec cmdpp(string()) -> ok.
cmdpp(CmdLine) ->
    Output = os:cmd(CmdLine),
    lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
		  string:tokens(Output, "\n")),
    ok.
