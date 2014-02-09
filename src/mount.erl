%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2014, Frank Hunleth
%%% @doc
%%% Simple interface to the mount(8) commandline utility.
%%% @end
%%% Created :  1 Jan 2014 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module(mount).

-export([mount/4, remount/2, umount/1]).

%%--------------------------------------------------------------------
%% @doc Run mount(8) to mount a file system
%%
%% This effectively runs the following in the shell:
%%   mount -t Type -o Options Device Directory
%%
%% @end
%%--------------------------------------------------------------------
-spec mount(string(), string(), atom(), [atom()]) -> ok | {error, non_neg_integer()}.
mount(Device, Directory, Type, Options) ->
    Arguments = options_to_args(Options) ++ ["-t", atom_to_list(Type), Device, Directory],
    subprocess:run("mount", Arguments).

%% Remount a file system
%% Example: remount("/", [rw]).
-spec remount(string(), [atom()]) -> ok | {error, non_neg_integer()}.
remount(Directory, Options) ->
    Arguments = options_to_args([remount | Options]) ++ [Directory],
    subprocess:run("mount", Arguments).

-spec umount(string()) -> ok | {error, non_neg_integer()}.
umount(Directory) ->
    subprocess:run("umount", [Directory]).

options_to_args([Option|T]) ->
    AllOptions = options_to_string(T, atom_to_list(Option)),
    ["-o", AllOptions];
options_to_args([]) ->
    [].

options_to_string([Option|T], Args) ->
    NewArgs = Args ++ "," ++ atom_to_list(Option),
    options_to_string(T, NewArgs);
options_to_string([], Args) ->
    Args.
