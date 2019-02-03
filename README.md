# LinEqSolver
LinEqSolver is an interactive console for solving/exploring linear equation systems.

Specifically, the LinEqSolver is designed to perform *gaussian elimination*.

The system is operated using commands. Every command must be suffixed by a ';' and a newline

Commands can span multiple lines and the system supports pasting multiple commands, given
that they live up to the requirements above.

Currently all commands that take row parameters use 0-indexing.

As of this build, the system supports the following list of commands:
```
- 'help;': displays this message.

- 'def <system name> <system definiton>;': defines a new equation system.
    Systems are defined by adding lines of the format:
      '[ x_0, x_1, ... | b ]'
    You can add as many lines as you want, just remember to terminate with a ';'.

- 'check <system name>;': naively checks if an equation system is solved (one possible solution is found)
  or if the system is inconsistent (still WIP and probably just states the obvious).

- 'trace <system name>;': lists all modifications to the system.

- 'textrace <system name> <style>;': performs trace, but outputs LaTeX tables.
  Variable names are determined by the style parameter.
  The system supports 4 styles:
  * 'geo': x, y and z (and nothing else)
  * 'alpha': a through z
  * 'one': one-indexed x-variables (starting at $x_1$)
  * 'zero': zero-indexed x-variables (starting at $x_0$)

- '<system name>.swap <row> <row>;': swaps two rows in the system.

- '<system name>.add <row> <row>;': adds two rows, stores the resulting row at the last parameter.

- '<system name>.add <row> <scalar> <row>;': adds two rows, but scales the first row before adding.

- '<system name>.scale <row> <scalar>;': scales a row.

- 'exit;' or 'quit;': exits the system.
```