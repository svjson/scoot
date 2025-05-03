# scoot.el

**scoot.el** is an Emacs extension for interacting with SQL databases via a running Scoot Server instance. It provides scratch buffers for writing and executing SQL interactively, with support for connection-specific metadata and result inspection.

Unlike `sql-mode`, scoot.el is designed for an iterative development workflow — letting you evaluate, inspect, and switch between multiple database contexts with minimal friction.

## Features

- SQL scratch buffers that persist between sessions
- Per-buffer or per-block connection configuration
- Evaluation of single statements or regions
- Commands for listing tables, schemas, databases
- Optional connection autoload via metadata blocks
- Non-intrusive defaults: no global keybindings

## Getting Started

Ensure a `scoot-server` instance is running (see the [scoot-server README](../scoot-server/)).

Then load the package in your Emacs config:

```elisp
(add-to-list 'load-path "/path/to/scoot-el")
(require 'scoot)
```

Create a new scratch buffer using:

```elisp
M-x scoot-new-scratch
```

This will open a new SQL buffer in `scoot-mode`, saved (by default) to `~/.scoot/scratches/`.

You can customize the location with `M-x customize-variable` or by setting it directly:

```elisp
(setq scoot-scratch-directory "~/my/sql-work")
```

## Scratch Buffer Format

You can include optional metadata blocks at the top or above each statement:

```sql
-- @connection-string: mssql+pyodbc://sa:PruttMuffin123@localhost:1433/tenants-leases?driver=ODBC+Driver+17+for+SQL+Server
-- @connection-name: test2

SELECT * FROM leases;
```

- `@connection-string` registers a new connection if needed
- `@connection-name` refers to an already-registered connection
- If both are provided, the string is used to register (or re-register) the given name
- Multiple metadata blocks can exist in the same file; the one nearest *above* a query is used

## Keybindings (in `scoot-mode`)

| Binding         | Action                           |
|-----------------|----------------------------------|
| `C-c C-c`       | Evaluate SQL statement before point |
| `C-c C-r`       | Evaluate region                  |
| `C-c s d`       | List databases                   |
| `C-c s s`       | List schemas                     |
| `C-c s t`       | List tables                      |
| `C-c d t`       | Describe table at point          |

No global bindings are defined — you should bind `scoot-new-scratch` to a key of your choice if desired.

Example:
```elisp
(global-set-key (kbd "C-c s n") #'scoot-new-scratch)
```

## Customizable variables

| Custom Variable                        | Default               | Type      | Descripton/Purpose                                                                           |
|----------------------------------------|-----------------------|-----------|----------------------------------------------------------------------------------------------|
| `scoot-scratch-directory`              | "~/.scoot/scratches/" | directory | Specifies the directory on disk to save Scoot scratch buffers.                               |
| `scoot-server-host`                    | "localhost"           | string    | The host name of the Scoot Server used for database interaction.                             |
| `scoot-server-port`                    | 8224                  | integer   | The port number of the Scoot Server used for database interaction.                           |
| `scoot-server-default-connection-name` | "default"             | string    | The name of the connection to use if none is specified / no context is available             |
| `scoot-auto-persist-connections`       | nil                   | boolean   | Determines if any connections created should be persisted to the active Server configuration |


## Dependencies

- Python 3.13
- A running `scoot-server`
- Emacs 27+ recommended
- [`request.el`](https://github.com/tkf/emacs-request) (HTTP client for Emacs)

## License

This package is licensed under the **GNU GPL v3.0**. See [`LICENSE`](LICENSE) for details.

