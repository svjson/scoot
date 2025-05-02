
# Scoot Server

**Scoot Server** provides an HTTP JSON API for interacting with SQL databases, powered by the shared `scoot-core` backend. 
It is primarily intended for use by the Scoot Emacs mode (`scoot-el`), but can also be used independently to build custom 
tools or remote database workflows.

## Features

- Exposes a REST-like API over HTTP
- Handles connection management and query execution via SQLAlchemy
- Lightweight and suitable for local or remote use
- JSON input/output for easy integration with editors and scripting tools

## Installation

```bash
# Inside the monorepo, from the root or scoot-server/
pip install .
```

## Running the Server

```bash
python -m scoot_server -p <port> -c <config_name>
```

### Flags

| Flag               | Description                                                |
|--------------------|------------------------------------------------------------|
| `-p <port>`        | Port to run the server on (default: 8221)                  |
| `-c <config-name>` | Use a named configuration. Defaults to `scoot_default`.    |

## API Overview

Scoot Server maintains stateful database connections that must be registered before use. This is done via the /api/connection endpoint by supplying a connection name and a SQLAlchemy-compatible URL. Subsequent API requests reference the registered connection by name as part of the request path.

This design allows the server to manage multiple active connections concurrently, each identified by name, and keeps client requests lightweight and reusable.

| Endpoint                                     | Method | Description                                  |
|----------------------------------------------|--------|----------------------------------------------|
| `/api/<connection_name>/tables`              | GET    | Returns list of all tables                   |
| `/api/<connection_name>/tables/<table_name>` | GET    | Returns schema info for a table              |
| `/api/<connection_name>/databases`           | GET    | Returns list of databases                    |
| `/api/<connection_name>/schemas`             | GET    | Returns list of schemas                      |
| `/api/<connection_name>/query`               | POST   | Executes a raw SQL query                     |
| `/api/connection`                            | POST   | Registers a new database connection          |
| `/api/connection`                            | GET    | Returns a list of all registered connections |

## Example Requests

### Execute Query

#### Request

```http
POST /api/default/query HTTP/1.1
Content-Type: application/json

{
  "sql": "SELECT * FROM users LIMIT 5"
}
```

#### Response

```json
{
  "columns": ["id", "name", "email"],
  "rows": [
    [1, "Alice", "alice@example.com"],
    [2, "Bob", "bob@example.com"]
  ]
}
```

### Register connection

A connection can be registered and activated with a request like the one below.
If the optional `persist` key is present in the body it will write the connection 
to the active server configuration.

#### Request

```http
POST /api/connection HTTP/1.1
Content-Type: application/json

{
  "name": "my-application",
  "url": "mssql+pyodbc://user:password@localhost:1433/my-app?driver=ODBC+Driver+17+for+SQL+Server",
  "persist": true
}
```

#### Response

```json
{
  "status": "ok"
}
```


## Notes

- Response format is optimized for programmatic consumption, not user-facing rendering.

## License

MIT License. See [`LICENSE`](LICENSE) for details.

---

© 2025 Sven Johansson. MIT Licensed.
