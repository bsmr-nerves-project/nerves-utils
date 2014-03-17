# nerves-utils
[![Build Status](https://travis-ci.org/nerves-project/nerves-utils.png)](https://travis-ci.org/nerves-project/nerves-utils)

This project contains useful Erlang modules for working with nerves
projects. None of this is substantial enough to warrant its own project
yet, but until then, it needs a home to prevent a lot of copy/paste
between projects.

## fatfs

This is a wrapper around mtools to support reading and writing to FAT file
partitions without requiring that they be mounted. This is useful for firmware
updates since images can be manipulated without requiring root permissions.

## fwprogrammer

This utility knows how to apply firmware update files created by fwtool. It is
used to run in-place upgrades on target systems.

## meminfo

This is a simple utility for parsing /proc/meminfo. It is useful for monitoring
the total system memory usage.

## mount

This file wraps calls to mount(2). It is useful for mounting, unmounting
and remounting filesystems.

## subprocess

This file contains utilities for dealing with OS processes. This includes
running processes, capturing their output and listing the active processes
(aka ps).

