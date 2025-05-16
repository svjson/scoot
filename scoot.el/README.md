# scoot.el

**scoot.el** is an Emacs extension for interacting with SQL databases via a running Scoot Server instance. It provides scratch buffers for writing and executing SQL interactively, with support for connection-specific metadata and result inspection.

Unlike `sql-mode`, scoot.el is designed for an iterative development workflow — letting you evaluate, inspect, and switch between multiple database contexts with minimal friction.

## Features

- SQL scratch buffers that persist between sessions
- Per-buffer or per-block connection configuration
- Interactive result set tables
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

This will open a new SQL buffer in `scoot-scratch-mode`, saved (by default) to `~/.scoot/scratches/`.

You can customize the location with `M-x customize-variable` or by setting it directly:

```elisp
(setq scoot-scratch-directory "~/my/sql-work")
```

No global keybindings are defined — if you want global keybindings open scoot scratches you should bind `scoot-new-scratch` to a key of your choice.

Example:
```elisp
(global-set-key (kbd "C-c s n") #'scoot-new-scratch)
```

Existing scratch buffers can be listed and recalled by invoking `scoot-open-scratch`.

```elisp
M-x scoot-open-scratch
```

## Scratch Mode (scoot-scratch-mode)

Scratch Mode provides a flexible workspace for writing, testing, and executing SQL statements 
directly from Emacs. This mode is particularly useful for exploring or updating databases 
without the need for separate query tools.


### Creating a Scratch Buffer

To open a new scratch buffer, use the command scoot-new-scratch. By default, this inserts 
connection details at the top of the buffer as comments, allowing you to define or refer 
to database connections.

### Scratch Buffer Structure

A scratch buffer primarily consists of SQL statements and optional connection annotations. 
The SQL syntax is based on the dialect of the connected database server, but scoot.el also 
recognizes specific configuration annotations within ANSI SQL-style comments.

Example:

```sql
-- @connection-string: mssql+pyodbc://sa:password@localhost:1433/my-database?driver=ODBC+Driver+17+for+SQL+Server
-- @connection-name: test2

SELECT * FROM accounts;

SELECT * FROM accounts WHERE postal_code = 'S-112 30';
```

- `@connection-string`: Defines or overrides the connection string for subsequent SQL statements.
- `@connection-name`: Associates a name with the connection string or references an existing connection by name.

If both annotations are provided, the connection string will register or update the named connection

### Multiple Connections in a Single Buffer

You can define multiple connection contexts within a single scratch buffer. A block of uninterrupted 
annotation lines defines the active connection context for the SQL statements that follow:

You can divide a scratch buffer into multiple sections using different connections.
A group of uninterrupted lines containing configuration annotations is used as the connection context for all statements that follow:

```sql
-- @connection-string: mssql+pyodbc://sa:password@localhost:1433/my-database?driver=ODBC+Driver+17+for+SQL+Server
-- @connection-name: dev-db

SELECT * FROM accounts;

SELECT * FROM accounts WHERE postal_code = 'S-112 30';

-- @connection-string: mssql+pyodbc://sa:password@localhost:14331/appdb?driver=ODBC+Driver+17+for+SQL+Server
-- @connection-name: myapp-test-env-tunnel

SELECT * FROM accounts WHERE postal_code = 'S-112 30';

-- @connection-name: dev-db

SELECT DISTINCT account_id FROM orders WHERE postal_code = 'S-112 30';
```

A connection context remains active until the next connection annotation block or the end of the buffer. This 
allows you to work with multiple databases seamlessly within the same buffer.

### Keybindings (in `scoot-scratch-mode`)

| Binding   | Action                              |
|-----------|-------------------------------------|
| `C-c C-c` | Evaluate SQL statement before point |
| `C-c C-r` | Evaluate region                     |
| `C-c s d` | List databases                      |
| `C-c s s` | List schemas                        |
| `C-c s t` | List tables                         |
| `C-c d t` | Describe table at point             |

### Customizable variables

| Custom Variable                  | Default               | Type               | Description/Purpose                                                    |
|----------------------------------|-----------------------|--------------------|------------------------------------------------------------------------|
| `scoot-auto-enable-scratch-mode` | t                     | boolean            | Automatically enable scoot-scratch-mode in scratch buffers.            |
| `scoot-scratch-directory`        | "~/.scoot/scratches/" | string (directory) | Directory on disk where Scoot scratch buffers are saved and read from. |

## Result Mode (scoot-result-mode)

Result Mode in `scoot.el` displays query results and metadata as an interactive 
ASCII-style table. This mode provides keybindings for quickly adding or removing 
WHERE-clause conditions based on the values in the table.

The mode is designed to streamline query adjustments, allowing rapid iteration and 
refinement without leaving the result view.

### Refining your query

The query backing the result set is fully editable. Modify the query as needed and
re-execute it with `C-c C-c`.

### Interacting with Results

In Result Mode, the table is interactive. You can navigate the cells using the 
arrow keys and execute commands to adjust query conditions directly from the table. 
The available commands are organized under a for adding conditions and r for removing 
them.

### Keybindings (in `scoot-result-mode`)

| Binding            | Context        | Action                                                                                                    |
|--------------------|----------------|-----------------------------------------------------------------------------------------------------------|
| `g`                | -              | Refresh the buffer                                                                                        |
| `TAB`              | Folded outline | Hide/show folded outline seection.                                                                        |
| `C-c C-c`          | -              | Execute query                                                                                             |
| `h` or `?`         | -              | Describe mode                                                                                             |
| `q`                | -              | Quit window                                                                                                          |
| `a w e` or `a w =` | Table cell     | Add this column/value to the WHERE-clause with an equals(=) expression.                                   |
| `a w n` or `a w !` | Table cell     | Add this column/value to the WHERE-clause with a not equals(!=) expression.                               |
| `a w >`            | Table cell     | Add this column/value to the WHERE-clause with a greater than(>) expression.                              |
| `a w g`            | Table cell     | Add this column/value to the WHERE-clause with a greater than or equals(>=) expression.                   |
| `a w <`            | Table cell     | Add this column/value to the WHERE-clause with a less than(<) expression.                                 |
| `a w l`            | Table cell     | Add this column/value to the WHERE-clause with a less than or equals(<=) expression.                      |
| `r w e` or `r w =` | Table cell     | Remove an existing equals(=) expression against this column/value from the WHERE-clause.                  |
| `r w n` or `r w !` | Table cell     | Remove an existing not equals(!=) expression against this column/value from the WHERE-clause.             |
| `r w >`            | Table cell     | Remove an existing greater than(>) expression against this column/value from the WHERE-clause..           |
| `r w g`            | Table cell     | Remove an existing greater than or equals(>=) expression against this column/value from the WHERE-clause. |
| `r w <`            | Table cell     | Remove an existing less than(<) expression against this column/value from the WHERE-clause.               |
| `r w l`            | Table cell     | Remove an existing less than or equals(<=) expression against this column/value from the WHERE-clause.    |

### Customizable variables (in `scoot-result-mode`)

You can adjust the appearance and behavior of the result table using these customizable 
variables:

| Custom Variable          | Default | Type   | Description/Purpose                                 |
|--------------------------|---------|--------|-----------------------------------------------------|
| `scoot-primary-key-icon` | "🔑"    | string | Icon used in table headers for primary key columns. |
| `scoot-foreign-key-icon` | "🗝️"    | string | Icon used in table headers for foreign key columns. |

### Regular variables (in `scoot-result-mode`)

| Variable                                     | Default                             | Type   | Description/Purpose                                                                  |
|----------------------------------------------|-------------------------------------|--------|--------------------------------------------------------------------------------------|
| `scoot-generate-result-buffer-name-function` | 'scoot-result--generate-buffer-name | symbol | Can be overriden to control the naming scheme for Scoot's result buffers             |
| `scoot-result-default-buffer-name`           | "*scoot result*                     | string | Default result buffer name to use name generation fails, is disabled or returns nil. |


## Commands for global use

| Command                | Args                                       | Description                                                                                                                                                                              |
|------------------------|--------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `scoot-new-scratch`    |                                            | Create and open a new scratch buffer.                                                                                                                                                    |
| `scoot-open-scratch`   |                                            | Open a scratch buffer from disk. Will list all `.scoot`-files in `scoot-scratch-directory`.                                                                                              |
| `scoot-start-server`   |                                            | Start the scoot-server                                                                                                                                                                   |
| `scoot-stop-server`    |                                            | Stop the running scoot-server                                                                                                                                                            |
| `scoot-restart-server` |                                            | Restart the rhe running scoot-server                                                                                                                                                     |
| `scoot-ensure-server`  | (&optional `force-start`)                  | Start the server if it is not running. If `scoot-auto-start-server` is set to `nil`, this will not make attempt to start the server unless a non-nil value is provided for `force-start` |
| `scoot-list-tables`    |                                            | List tables (if no contextual connection is available, this opens a prompt to select a connection.)                                                                                      |
| `scoot-describe-table` | (&optional `table-name` `connection-name`) | Describe a table (if table-name and/or connection-name is not provided, this opens prompts.)                                                                                             |
| `scoot-list-databases` |                                            | List databases (if no contextual connection is available, this opens a prompt to select a connection.)                                                                                   |
| `scoot-list-schemas`   |                                            | List schemas (if no contextual connection is available, this opens a prompt to select a connection.)                                                                                     |


## Customizable variables (general)

### Server Management

| Custom Variable              | Default          | Type    | Description/Purpose                                                                                 |
|------------------------------|------------------|---------|-----------------------------------------------------------------------------------------------------|
| `scoot-auto-start-server`    | t                | boolean | Enable/disable auto-start of scoot-server when required.                                            |
| `scoot-server-config-name`   | "default"        | string  | The server configuration name/id to use for the managed server.                                     |
| `scoot-server-host`          | "localhost"      | string  | The host name of the Scoot Server used for database interaction.                                    |
| `scoot-server-port`          | 8224             | integer | The port number of the Scoot Server used for database interaction.                                  |
| `scoot-show-server-buffer`   | nil              | boolean | Enable/disable server output in *scoot-server* buffer                                               |
| `scoot-server-buffer-name`   | "*scoot-server*" | string  | Name of the Scoot server buffer, if enabled by `scoot-show-server-buffer`                           |
| `scoot-server-start-timeout` | 5                | number  | Timeout, in seconds, to wait for the scoot-server to become available after issuing a start command |

### Connection Management

| Custom Variable                        | Default   | Type    | Description/Purpose                                                                          |
|----------------------------------------|-----------|---------|----------------------------------------------------------------------------------------------|
| `scoot-server-default-connection-name` | "default" | string  | The name of the connection to use if none is specified / no context is available             |
| `scoot-auto-persist-connections`       | nil       | boolean | Determines if any connections created should be persisted to the active Server configuration |

### xref Symbols

These custom variables are used for displaying xref-definitions, and also serve as hints
for the xref backend when determining definition type.

| Custom Variable            | Default                           | Type   |
|----------------------------|-----------------------------------|--------|
| `scoot-xref-database-icon` | " " / nf-fa-database / f1c0      | string |
| `scoot-xref-table-icon`    | " " / nf-fa-table_cells / f00a   | string |
| `scoot-xref-column-icon`   | " " / nf-fa-table_columns / f0db | string |


## Dependencies

- Python 3.13 / pip / virtualenv
- Emacs 27+ recommended
- [`request.el`](https://github.com/tkf/emacs-request) (HTTP client for Emacs)

## License

This package is licensed under the **GNU GPL v3.0**. See [`LICENSE`](LICENSE) for details.

