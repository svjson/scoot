
### Scoot Core

**scoot-core** is a lightweight Python library that provides a shared SQLAlchemy-based backend for both the Scoot CLI and Scoot HTTP server. It serves as the common foundation for connecting to databases and executing SQL queries in a consistent and reusable way.

## Features

- Thin abstraction around SQLAlchemy
- Reusable database connection management
- Unified query interface for CLI and server use

This package does not aim to replace SQLAlchemy or necessarily abstract away its API — it simply centralizes configuration and basic operations for use by other Scoot components.

## Usage

This library is not intended for standalone use. It is consumed internally by:

- [`scoot-server`](../scoot-server/)
- [`scoot-cli`](../scoot-cli/)

## License

MIT License. See [`LICENSE`](LICENSE) for details.

---

© 2025 Sven Johansson. MIT Licensed.
