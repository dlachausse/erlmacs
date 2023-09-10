# erlmacs
Simple script to update your .emacs file for Erlang development

erlmacs automatically configures and updates your .emacs file with support for the emacs mode that is included with Erlang/OTP.
It frees you from having to locate the installation directory of Erlang/OTP and its bundled emacs mode.

## Requirements
- Erlang (with the escript command in the PATH)
- Emacs

## Installation
- Download or save the `erlmacs.escript` file
- If you're using a UNIX-like system such as macOS, Linux, or BSD make this script executable: `chmod +x erlmacs.escript`
- Otherwise execute it as follows: `escript erlmacs.escript [command]`

## Example usage
`./erlmacs.escript [command]`
or
`escript erlmacs.escript [command]`

where [command] is one of the following:

```
install     Add erlmacs configuration to .emacs file
remove      Remove erlmacs configuration from .emacs file
update      Update existing erlmacs configuration in .emacs file
```
