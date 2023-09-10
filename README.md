# erlmacs
Simple script to update your .emacs file for Erlang development

erlmacs automatically configures and updates your .emacs file with support for the emacs mode that is included with Erlang/OTP.
It frees you from having to locate the installation directory of Erlang/OTP and its bundled emacs mode.

## Requirements
- Erlang (with the escript command in the PATH)
- Emacs

## Installation
- Download or save the `erlmacs` file
- If you're using a UNIX-like system such as macOS, Linux, or BSD make this script executable: `chmod +x erlmacs`
- Otherwise execute it as follows: `escript erlmacs [command]`

## Example usage
`./erlmacs [command]`
or
`escript erlmacs [command]`

where [command] is one of the following:

```
install     Add erlmacs configuration to .emacs file
remove      Remove erlmacs configuration from .emacs file
update      Update existing erlmacs configuration in .emacs file
```
