#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Copyright (c) 2023, Darren L. LaChausse
%% ALL RIGHTS RESERVED
%%
%% Released under the MIT open source license, see LICENSE for details.
%%

main(_) ->
    % User's home directory
    {ok, [[HomeDir]]} = init:get_argument(home),
    % Erlang installation root directory
    {ok, [[ErlRoot]]} = init:get_argument(root),
    % Location of the "erlang.el" file included with Erlang
    ErlangDotEl = filelib:wildcard(ErlRoot ++ "/lib/tools-*/emacs"),
    % Location of the user's ".emacs" file
    DotEmacs = HomeDir ++ "/.emacs",
    
    % Configuration to append to user's .emacs file...
    Config = 
"\n;;;ERLMACS begin;;;\n"
"(setq load-path (cons \"" ++ ErlangDotEl ++ "\" load-path))\n"
"(setq erlang-root-dir \"" ++ ErlRoot ++ "\")\n"
"(setq exec-path (cons \"" ++ ErlRoot ++ "/bin\" exec-path))\n"
"(require 'erlang-start)\n"
";;;ERLMACS end;;;\n",

    io:format("Appending the following:~s\nto ~s...\n", [Config, DotEmacs]),
    
    % Append the configuration to the user's .emacs file...
    file:write_file(DotEmacs, Config, [append]),

    io:format("Done.\n\n").
