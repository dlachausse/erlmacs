#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Copyright (c) 2023, Darren L. LaChausse
%% ALL RIGHTS RESERVED
%%
%% Released under the MIT open source license, see LICENSE for details.
%%

% Print usage information
usage() ->
    io:format(
      "Example usage:\n"
      "\terlmacs.escript [command]\n\n"
      "where [command] is one of the following:\n"
      "\tinstall\t\tAdd configuration to .emacs file\n\n").

% Generate the emacs configuration file contents
gen_emacs_config() ->
    % Erlang installation root directory
    {ok, [[ErlRoot]]} = init:get_argument(root),
    % Location of the "erlang.el" file included with Erlang
    ErlangDotEl = filelib:wildcard(ErlRoot ++ "/lib/tools-*/emacs"),
    
    % Configuration to append to user's .emacs file...
	"\n;;;ERLMACS begin;;;\n"
	"(setq load-path (cons \"" ++ ErlangDotEl ++ "\" load-path))\n"
	"(setq erlang-root-dir \"" ++ ErlRoot ++ "\")\n"
	"(setq exec-path (cons \"" ++ ErlRoot ++ "/bin\" exec-path))\n"
	"(require 'erlang-start)\n"
	";;;ERLMACS end;;;\n".

% Get user's home directory
home_dir() ->
    {ok, [[HomeDir]]} = init:get_argument(home),
    HomeDir.

% Return the path to the .emacs config file
get_dot_emacs() ->
    home_dir() ++ "/.emacs".

install() ->
    % Location of the user's ".emacs" file
    DotEmacs = get_dot_emacs(),

    Config = gen_emacs_config(),
    
    io:format("Appending the following:~s\nto ~s...\n", [Config, DotEmacs]),
    
    % Append the configuration to the user's .emacs file...
    file:write_file(DotEmacs, Config, [append]),

    io:format("Done.\n\n").

main(["install"]) ->
    install();

main(_) ->
    usage().
