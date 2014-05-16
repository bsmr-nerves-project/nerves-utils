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
-module(fatfs).

-export([read_file/2, write_file/3, rm_file/2, mv_file/3]).

%% Read, write, and manipulate files on a FAT filesystem
%% This module requires mtools to be installed to work.

%% Read the contents of a file on a FAT file system
-spec read_file({string(), non_neg_integer()}, string()) -> {ok, binary()} | {error, term()}.
read_file({RawImage, FatPartitionOffset}, DosFilename) ->
    case subprocess:run("mtype", ["-i",
				  RawImage ++ "@@" ++ integer_to_list(FatPartitionOffset),
				  "::" ++ DosFilename]) of
	{ok, Contents} ->
	    {ok, Contents};
	{error, Reason} ->
	    {error, {mtype_failed, Reason}}
    end.

-spec write_file({string(), non_neg_integer()}, string(), iodata()) -> ok | {error, term()}.
write_file({RawImage, FatPartitionOffset}, DosFilename, Contents) ->
    % subprocess:run can't shut down stdin when done, so we have to
    % create a temporary file rather than piping.
    TempFilename = temp_filename(),
    ok = file:write_file(TempFilename, Contents),
    case subprocess:run("mcopy",
			["-i", RawImage ++ "@@" ++ integer_to_list(FatPartitionOffset),
			 "-o", % overwrite without asking
			 TempFilename,
			 "::" ++ DosFilename]) of
	{ok, _} ->
	    file:delete(TempFilename),
	    ok;
	{error, Reason} ->
	    file:delete(TempFilename),
	    {error, {mcopy_failed, Reason}}
    end.

% Delete a file on a FAT file system
-spec rm_file({string(), non_neg_integer()}, string()) -> ok | {error, term()}.
rm_file({RawImage, FatPartitionOffset}, DosFilename) ->
    case subprocess:run("mdel", ["-i",
				  RawImage ++ "@@" ++ integer_to_list(FatPartitionOffset),
				  "::" ++ DosFilename]) of
	{ok, _} ->
	    ok;
	{error, Reason} ->
	    {error, {mdel_failed, Reason}}
    end.

% Move a file on a FAT file system
-spec mv_file({string(), non_neg_integer()}, string(), string()) -> ok | {error, term()}.
mv_file({RawImage, FatPartitionOffset}, FromDosFilename, ToDosFilename) ->
    case subprocess:run("mdel", ["-i",
				  RawImage ++ "@@" ++ integer_to_list(FatPartitionOffset),
				  "::" ++ FromDosFilename,
				  "::" ++ ToDosFilename
				]) of
	{ok, _} ->
	    ok;
	{error, Reason} ->
	    {error, {mdel_failed, Reason}}
    end.

-spec temp_filename() -> string().
temp_filename() ->
    Unique = erlang:phash2(make_ref()),
    lists:flatten(io_lib:format("/tmp/~p.tmp", [Unique])).
