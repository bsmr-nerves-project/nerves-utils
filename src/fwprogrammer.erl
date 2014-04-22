%% The MIT License (MIT)
%%
%% Copyright (c) 2013 Frank Hunleth
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
-module(fwprogrammer).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0, program/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% Information about the firmware
-record(fwinfo,
        { destpath,
	  writer,
	  ziphandle,
	  instructions
	  }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term() }.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @doc program/3 runs the firmware update on the specified destination.
%%
%% To update the SDCard on the Beaglebone in the shell:
%%
%% fwprogrammer:start_link().
%% fwprogrammer:program("/tmp/bbb.fw", autoupdate, "/dev/mmcblk0").
%%
%% @end
-spec program(string(), atom(), string()) -> ok.
program(FirmwarePath, UpdateType, DestinationPath) ->
    gen_server:call(?SERVER,
		    {program, FirmwarePath, UpdateType, DestinationPath},
		    120000).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({program, FirmwarePath, UpdateType, DestinationPath}, _From, State) ->
    {ok, Writer} = open_destination(DestinationPath),
    {ok, ZipHandle} = zip:zip_open(FirmwarePath, [memory]),
    {ok, {_, InstructionsBin}} = zip:zip_get("instructions.json", ZipHandle),
    Instructions = jsx:decode(InstructionsBin, [{labels, atom}]),

    Fwinfo = #fwinfo{destpath=DestinationPath,
		     writer=Writer,
		     ziphandle=ZipHandle,
		     instructions=Instructions},

    run_update(UpdateType, Fwinfo),

    close_destination(Writer),
    ok = zip:zip_close(ZipHandle),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
run_update(UpdateType, Fwinfo) ->
    case proplists:get_value(UpdateType, Fwinfo#fwinfo.instructions) of
	undefined -> exit(unsupported_upgrade);
	Update -> run_commands(Update, Fwinfo)
    end.

run_commands([], _Fwinfo) -> ok;
run_commands([Command|T], Fwinfo) ->
    case run_command(Command, Fwinfo) of
	done ->
	    ok;
	keep_going ->
	    run_commands(T, Fwinfo)
    end.

% Run a command in the instructions.json list
run_command([<<"pwrite">>, Path, Location, Size], Fwinfo) ->
    % Write the specified file in the update package to a location in
    % the target image.
    {ok, {_, Data}} = zip:zip_get(binary_to_list(Path), Fwinfo#fwinfo.ziphandle),
    Size = byte_size(Data),
    ok = pwrite(Fwinfo#fwinfo.writer, Location, Data),
    keep_going;
run_command([<<"fat_write_file">>, Path, Location, FatFilename], Fwinfo) ->
    % Write the specified Path to the file FatFilename in the
    % FAT filesystem at Location.
    {ok, {_, Data}} = zip:zip_get(binary_to_list(Path), Fwinfo#fwinfo.ziphandle),
    ok = fatfs:write_file({Fwinfo#fwinfo.destpath, Location}, binary_to_list(FatFilename), Data),
    keep_going;
run_command([<<"compare_and_run">>, Path, Location, Size, SuccessUpdateType], Fwinfo) ->
    % Compare the raw contents of a location in the target image with the
    % the specified file and "branch" to other update instructions if a match
    {ok, {_, CheckData}} = zip:zip_get(binary_to_list(Path), Fwinfo#fwinfo.ziphandle),
    Size = byte_size(CheckData),
    {ok, ActualData} = pread(Fwinfo#fwinfo.writer, Location, Size),
    if
	ActualData =:= CheckData ->
	    run_update(binary_to_atom(SuccessUpdateType, latin1), Fwinfo),
	    done;
	true ->
	    keep_going
    end;
run_command([<<"fat_compare_and_run">>, Path, Location, FatFilename, SuccessUpdateType], Fwinfo) ->
    % Compare the contents of a file in a FAT partition located at Location with
    % file in the update archive. If they match, then "branch" to other update
    % instructions.
    {ok, {_, CheckData}} = zip:zip_get(binary_to_list(Path), Fwinfo#fwinfo.ziphandle),
    {ok, ActualData} = fatfs:read_file({Fwinfo#fwinfo.destpath, Location}, binary_to_list(FatFilename)),
    if
	ActualData =:= CheckData ->
	    run_update(binary_to_atom(SuccessUpdateType, latin1), Fwinfo),
	    done;
	true ->
	    keep_going
    end;
run_command([<<"fail">>, Message], _Fwinfo) ->
    exit(binary_to_list(Message)).

%% Since Erlang can't read or write to device files directly, we
%% may need to use a helper program like mmccopy. The following
%% code figures out what to do.
-spec open_destination(string()) -> {ok, term()} | {error, term()}.
open_destination(DestinationPath) ->
    case file:read_file_info(DestinationPath) of
	{error, enoent} ->
	    % File doesn't exist, so create it.
	    {ok, Handle} = file:open(DestinationPath, [read,write,binary]),
	    {ok, {file, Handle}};
	{error, Reason} ->
	    % Something's wrong
	    io:format("Error: Problem with ~p~n", [DestinationPath]),
	    {error, Reason};
	{ok, #file_info{access = read}} ->
	    % If the file is readonly, then it's also no good
	    io:format("Error: ~p is readonly~n", [DestinationPath]),
	    {error, readonly};
	{ok, #file_info{type = directory}} ->
	    % A directory doesn't work either
	    io:format("Error: ~p is directory. Expecting a file or device.~n", [DestinationPath]),
	    {error, eisdir};
	{ok, #file_info{type = regular}} ->
	    % If a regular file, then just use Erlang
	    {ok, Handle} = file:open(DestinationPath, [read,write,binary]),
	    {ok, {file, Handle}};
	{ok, #file_info{type = device}} ->
	    % Device file. Erlang's file module won't allow writes
	    % to devices, so check for helpers
	    open_destination_helper(DestinationPath)
    end.

open_destination_helper(DestinationPath) ->
    case os:find_executable("mmccopy") of
	false ->
	    {error, nohelper};
	Mmccopy ->
	    {ok, {mmccopy, Mmccopy, DestinationPath}}
    end.

-spec pwrite(term(), non_neg_integer(), binary()) -> ok | {error, term()}.
pwrite({file, Handle}, Location, Data) ->
    file:pwrite(Handle, Location, Data);
pwrite({mmccopy, Mmccopy, DestinationPath}, Location, Data) ->
    DataSize = byte_size(Data),
    Args = ["-d", DestinationPath,
	    "-s", integer_to_list(DataSize),
	    "-o", integer_to_list(Location),
	    "-q",
	    "-"],
    {ok,_} = subprocess:run(Mmccopy, Args, Data),
    ok.

-spec pread(term(), non_neg_integer(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
pread({file, Handle}, Location, Number) ->
    file:pread(Handle, Location, Number);
pread({mmccopy, Mmccopy, DestinationPath}, Location, Number) ->
    Args = ["-r",
	    "-d", DestinationPath,
	    "-s", integer_to_list(Number),
	    "-o", integer_to_list(Location),
	    "-q",
	    "-"],
    subprocess:run(Mmccopy, Args).

close_destination({file, Handle}) ->
    file:close(Handle);
close_destination({mmccopy, _Mmccopy, _DestinationPath}) ->
    ok.
