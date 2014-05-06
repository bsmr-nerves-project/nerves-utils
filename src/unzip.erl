-module(unzip).

-export([read_file/2, copy_to_file/5]).

-spec read_file(string(), string()) -> {ok, binary()} | {error, integer()}.
read_file(ZipFilename, FileInZip) ->
    Port = open_unzipper_port(ZipFilename, FileInZip),
    read_till_done(Port, <<>>).

read_till_done(Port, Data) ->
    receive
	{Port, {data, NewData}} ->
	    ConcatenatedData = <<Data/binary, NewData/binary>>,
	    read_till_done(Port, ConcatenatedData);
	{Port, {exit_status, 0}} ->
	    {ok, Data};
	{Port, {exit_status, ExitStatus}} ->
	    {error, ExitStatus}
    end.

open_unzipper_port(ZipFilename, FileInZip) ->
    Unzip = os:find_executable("unzip"),
    open_port({spawn_executable, Unzip},
	      [stream,
	       {args, ["-p", ZipFilename, FileInZip]},
	       exit_status,
	       binary]).

open_mmccopy_port(DestFile, DestOffset, ExpectedByteCount) ->
    Mmccopy = os:find_executable("mmccopy"),
    open_port({spawn_executable, Mmccopy},
	      [stream,
	       {args, ["-d", DestFile,
		       "-o", integer_to_list(DestOffset),
		       "-s", integer_to_list(ExpectedByteCount),
		       "-q",
		       "-w",
		       "-"]},
	       exit_status,
	       binary]).


-spec copy_to_file(string(), string(), non_neg_integer(), string(), non_neg_integer()) -> ok | {error, term()}.
copy_to_file(ZipFilename, FileInZip, ExpectedByteCount, DestFile, DestOffset) ->
    UnzipPort = open_unzipper_port(ZipFilename, FileInZip),
    MmccopyPort = open_mmccopy_port(DestFile, DestOffset, ExpectedByteCount),
    copy_till_done(UnzipPort, MmccopyPort, ExpectedByteCount).

wait_for_mmccopy(MmccopyPort) ->
    receive
	{MmccopyPort, {data, _}} ->
	    {error, {mmccopy, unexpected_data}};
	{MmccopyPort, {exit_status, 0}} ->
	    ok;
	{MmccopyPort, {exit_status, ExitStatus}} ->
	    {error, {mmccopy, ExitStatus}}
    end.

copy_till_done(UnzipPort, MmccopyPort, ByteCount) ->
    receive
	{UnzipPort, {data, Data}} ->
	    DataLength = byte_size(Data),
	    NewByteCount = ByteCount - DataLength,
	    if
		NewByteCount >= 0 ->
		    MmccopyPort ! {self(), {command, Data}},
		    copy_till_done(UnzipPort, MmccopyPort, NewByteCount);
		true ->
		    {error, {mmccopy, too_many_bytes}}
	    end;
	{UnzipPort, {exit_status, 0}} ->
	    case ByteCount of
		0 -> wait_for_mmccopy(MmccopyPort);
		_ -> {error, too_few_bytes}
	    end;
	{UnzipPort, {exit_status, ExitStatus}} ->
	    MmccopyPort ! {self(), close},
	    {error, ExitStatus};
	{MmccopyPort, {data, _}} ->
	    UnzipPort ! {self(), close},
	    MmccopyPort ! {self(), close},
	    {error, {mmccopy, unexpected_data}};
	{MmccopyPort, {exit_status, _}} ->
	    UnzipPort ! {self(), close},
	    {error, {mmccopy, unexpected_close}}
    end.
