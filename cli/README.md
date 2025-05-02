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

### Flags

All commands require a database connection, either passed through the --url 
flag or present in a persistent configuration.

| Flag               | Description                                                |
|--------------------|------------------------------------------------------------|
| `--url <url>`      | Use a connection string/url for connecting to a  database. |
| `-c <config-name>` | Use a named configuration. Defaults to `scoot_default`.    |

### Commands Overview

| Command          | Description                         |
|------------------|-------------------------------------|
| `table list`     | Lists all tables                    |
| `table describe` | Shows column info for a given table |
| `db list`        | Lists databases (where supported)   |
| `schema list`    | Lists schemas                       |
| `query`          | Executes a raw SQL query            |


#### List databases
```bash
scoot --url postgres://user:pass@localhost/mydb db list
```

#### List schemas
```bash
scoot --url postgres://user:pass@localhost/mydb schema list
```

#### List all tables
```bash
scoot --url sqlite:///my.db table list
```

#### Describe a specific table
```bash
scoot --url sqlite:///my.db table describe users
```

#### Run a raw SQL Query
```bash
scoot --url sqlite:///my.db query "SELECT * FROM users LIMIT 5"
```

## Notes

- `--url` is always required and should be a SQLAlchemy-compatible database URI.
- Output is currently printed to stdout and is primarily designed for developer use and scripting.

## License

MIT License. See [`LICENSE`](LICENSE) for details.

---

© 2025 Sven Johansson. MIT Licensed.
