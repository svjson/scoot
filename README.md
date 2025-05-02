
# Scoot

**Scoot** is a modular toolkit for exploring and interacting with databases from 
multiple interfaces. It provides a powerful Python backend, an HTTP API server, 
a CLI client, and a rich Emacs integration for SQL development.

As a modular, extensible toolkit for working with databases — its real strength lies in 
its powerful Emacs integration. 
At its heart is a versatile Emacs mode designed to support rapid, iterative development of 
applications with a database backend. Unlike traditional tools like sql-mode, 
Scoot aims to provide a far more interactive and context-aware experience:

- Evaluating queries inline
- Inspecting results instantly
- Interactive result sets
- Integrating seamlessly with project-specific database configurations.

While all these goals are not currently fulfilled in this early version, development is ongoing.

Scoot is built from the ground up to be developer-friendly, whether you're exploring schema changes, prototyping queries, or integrating database work into your daily programming workflow.

## Project Overview

Scoot is composed of four interrelated subprojects:

| Subproject      | Description                                                                 |
|-----------------|-----------------------------------------------------------------------------|
| `scoot-core`    | Core Python library for database connections and query execution via SQLAlchemy. |
| `scoot-server`  | HTTP JSON API that exposes Scoot functionality over a local or remote server. |
| `scoot-cli`     | Command-line interface for running SQL queries and interacting with configured databases. |
| `scoot-el`      | Emacs mode that connects to a running Scoot server, enabling SQL editing, execution, and result inspection. |

## Use Cases

- Query and explore SQL databases through Emacs with rich interactive features.
- Build tools and integrations using the Scoot HTTP API.
- Automate queries and reporting using the command-line tool.
- Develop database-driven workflows on top of a clean, extensible Python core.

## Architecture

```
[scoot-el]  ─┬─> [scoot-server] ─> [scoot-core]
[scoot-cli] ─┘
```

- **scoot-core** handles all low-level database logic using SQLAlchemy.
- **scoot-server** wraps the core in an HTTP/JSON API.
- **scoot-cli** uses the core directly for terminal use.
- **scoot-el** connects to the server for seamless Emacs-based interaction.

## Getting Started

The Makefile provided in the repository root will create a venv and setup all Python subprojects.

Depending on your needs, follow the relevant README:

- **Emacs integration**: [`scoot-el/README.md`](scoot.el/README.md)
- **HTTP server**: [`scoot-server/README.md`](server/README.md)
- **Command-line tool**: [`scoot-cli/README.md`](cli/README.md)
- **Core library**: [`scoot-core/README.md`](core/README.md)

## Development

To contribute or set up a development environment, refer to the individual subproject READMEs.

## Licensing

This repository contains multiple subprojects with different licenses:

- `scoot-el`: GNU General Public License v3.0 only (GPL-3.0-only)
- `scoot-core`, `scoot-server`, `scoot-cli`: MIT License

See each subdirectory's `LICENSE` file for details.

---

© 2025 Sven Johansson. GPL-3.0 applies to scoot-el; MIT applies to core/server/CLI. See subproject LICENSE files.
