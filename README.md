# erlmacs
Simple script to update your .emacs file for Erlang development

## Requirements
- Erlang (with the escript command in the PATH)
- Emacs

## Installation
- Download or save the erlmacs.escript file
- If you're using a UNIX^TM-like system such as MacOS, Linux, or BSD make this script executable: `chmod +x erlmacs.escript`
- Otherwise execute it as follows: `escript erlmacs.escript [command]`

## Example usage
`erlmacs.escript [command]`

where [command] is one of the following:

```
install     Add erlmacs configuration to .emacs file
remove      Remove erlmacs configuration from .emacs file
```
