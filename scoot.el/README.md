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

## Requirements

- Python3.13 / pip / virtual env (for managing and running the [scoot-server](../scoot-server/)).
- request.el [request.el](https://github.com/tkf/emacs-request) (will be installed from MELPA if using [straight.el](https://github.com/radian-software/straight.el)

## Installation

You can either install `scoot.el` via `straight.el` (optionally together with use-package) or
manually clone the repository and load the package.

`scoot.el` will install the Python backend in a virtualenv automatically in both cases.

### Using straight.el

```elisp
(straight-use-package
 '(scoot :host github :repo "svjson/scoot" :files ("scoot.el/*.el)")
 (require 'scoot)
```

### Using use-package + straight.el

Because the Scoot project uses a monorepo structure, which is not supported by 
`use-package` + `package.el`, using straight.el is required.

```elisp
(use-package scoot
  :straight (:host github :repo "svjson/scoot" :files ("scoot.el/*.el")))
```

### Manual installation

If you prefer to install the package manually, or can't use straight.el, manual installation
is simple and straight forward.

1. Clone the repository to a location of your choosing, ie `~/.emacs.d/site-lisp/`.

2. Add the package to your `load-path`. Note that the package is not located in the repository root.

   ```elisp
   (add-to-list 'load-path "~/.emacs.d/site-lisp/scoot/scoot.el")
   (require 'scoot)
   ```

## Getting Started

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
-- @connection-string: mssql+pyodbc://sa:password@localhost:1433/my-database?driver=ODBC+Driver+17+for+SQL+Server
-- @connection-name: test2

SELECT * FROM accounts;
```

- `@connection-string` registers a new connection if needed
- `@connection-name` refers to an already-registered connection
- If both are provided, the string is used to register (or re-register) the given name
- Multiple metadata blocks can exist in the same file; the one nearest *above* a query is used

## Keybindings (in `scoot-mode`)

| Binding   | Action                              |
|-----------|-------------------------------------|
| `C-c C-c` | Evaluate SQL statement before point |
| `C-c C-r` | Evaluate region                     |
| `C-c s d` | List databases                      |
| `C-c s s` | List schemas                        |
| `C-c s t` | List tables                         |
| `C-c d t` | Describe table at point             |

No global bindings are defined — you should bind `scoot-new-scratch` to a key of your choice if desired.

Example:
```elisp
(global-set-key (kbd "C-c s n") #'scoot-new-scratch)
```

## Customizable variables

### Server Management

| Custom Variable              | Default     | Type    | Description/Purpose                                                                                 |
|------------------------------|-------------|---------|-----------------------------------------------------------------------------------------------------|
| `scoot-auto-start-server`    | t           | boolean | Enable/disable auto-start of scoot-server when required.                                            |
| `scoot-server-config-name`   | "default"   | string  | The server configuration name/id to use for the managed server.                                     |
| `scoot-server-host`          | "localhost" | string  | The host name of the Scoot Server used for database interaction.                                    |
| `scoot-server-port`          | 8224        | integer | The port number of the Scoot Server used for database interaction.                                  |
| `scoot-show-server-buffer`   | nil         | boolean | Enable/disable server output in *scoot-server* buffer                                               |
| `scoot-server-start-timeout` | 5           | number  | Timeout, in seconds, to wait for the scoot-server to become available after issuing a start command |


### Connection Management

| Custom Variable                        | Default   | Type    | Description/Purpose                                                                          |
|----------------------------------------|-----------|---------|----------------------------------------------------------------------------------------------|
| `scoot-server-default-connection-name` | "default" | string  | The name of the connection to use if none is specified / no context is available             |
| `scoot-auto-persist-connections`       | nil       | boolean | Determines if any connections created should be persisted to the active Server configuration |


### Scratch Buffers

| Custom Variable                  | Default               | Type      | Description/Purpose                                                                   |
|----------------------------------|-----------------------|-----------|---------------------------------------------------------------------------------------|
| `scoot-auto-enable-scratch-mode` | t                     | boolean   | Enable/disable auto-enabling of scoot-scratch-mode when opening Scoot scratch buffers |
| `scoot-scratch-directory`        | "~/.scoot/scratches/" | directory | Specifies the directory on disk to save Scoot scratch buffers.                        |


## Dependencies

- Python 3.13 / pip / virtualenv
- Emacs 27+ recommended
- [`request.el`](https://github.com/tkf/emacs-request) (HTTP client for Emacs)

## License

This package is licensed under the **GNU GPL v3.0**. See [`LICENSE`](LICENSE) for details.

