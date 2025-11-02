# Scoot CLI

**Scoot CLI** is a lightweight command-line interface for exploring and
interacting with SQL databases using the shared `scoot-core` backend.

It supports listing tables, schemas, databases, describing tables, and
executing arbitrary SQL queries — all over a database connection specified via URL.

## Installation

```bash
# Inside the monorepo, from the root or scoot-cli/
pip install .
```

## Usage

Commands follow a resource/verb structure, usually in the form of:

```sh
$ scoot <resource> <verb>
```

### Resources

| Resource   | Verbs                  | Requires connection |
|------------|------------------------|---------------------|
| connection | set-default, list      | No                  |
| context    | list, use              | No                  |
| db         | list                   | Yes                 |
| schema     | list                   | Yes                 |
| table      | describe, export, list | Yes                 |

### Verbs

| Verb  | Requires connection |
|-------|---------------------|
| query | Yes                 |

#### Run a raw SQL Query
```bash
scoot --url sqlite:///my.db query "SELECT * FROM users LIMIT 5"
```

### `connection` verbs

| Verb        | Arguments         | Description                                             |
|-------------|-------------------|---------------------------------------------------------|
| set-default | <connection-name> | Set the default `connection` within the current context |
| list        |                   | List contexts                                           |

### `context` verbs

| Verb | Arguments      | Description                                                       |
|------|----------------|-------------------------------------------------------------------|
| list |                | List contexts                                                     |
| use  | <context-name> | Set the default `context`. Subsequent commands do not require -c. |

### `db` verbs

#### List databases
```bash
scoot --url postgres://user:pass@localhost/mydb db list
```

| Verb | Arguments | Description    |
|------|-----------|----------------|
| list |           | List databases |

### `schema` verbs

| Verb | Arguments | Description  |
|------|-----------|--------------|
| list |           | List schemas |

#### List schemas
```bash
scoot --url postgres://user:pass@localhost/mydb schema list
```

### `table` verbs

| Verb     | Arguments                                                     | Description                                   |
|----------|---------------------------------------------------------------|-----------------------------------------------|
| describe | <table-name>                                                  | Describe table                                |
| export   | <table-name> [-o <format>], [-f <filename>], [--include-data] | Export table definition and, optionally, data |
| list     |                                                               | List tables                                   |

#### List all tables
```bash
scoot --url sqlite:///my.db table list
```

#### Describe a specific table
```bash
scoot --url sqlite:///my.db table describe users
```

### Connection and Contxt Flags

All commands that read or write from a database requires a database connection, 
either passed through the --url flag or present in a persistent configuration or
through a stored context and connection.

| Flag                | Description                                                       |
|---------------------|-------------------------------------------------------------------|
| `--url <url>`       | Use a connection string/url for connecting to a database.         |
| `-c <context-name>` | Use a named configuration. Defaults to the currently set context. |

## Notes

- `--url` is always required and should be a SQLAlchemy-compatible database URI.
- Output is currently printed to stdout and is primarily designed for developer use and scripting.

## License

MIT License. See [`LICENSE`](LICENSE) for details.

---

© 2025 Sven Johansson. MIT Licensed.
