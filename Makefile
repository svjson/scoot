.PHONY: dev-setup test-backend test-core test-system test-unit test-emacs test-emacs-unit test
BACKEND ?= all

dev-setup:
	python -m venv .venv
	.venv/bin/pip install -e ./core
	.venv/bin/pip install -e ./cli
	.venv/bin/pip install -e ./server
	.venv/bin/pip install -r requirements-test.txt

test: test-system

test-unit:
	@echo "Running Unit Tests for all modules..."
	@.venv/bin/pytest --core --cli --server --emacs || exit 1

test-core:
	@echo "Running Core Tests..."
	@.venv/bin/pytest --core || exit 1

test-emacs:
	@echo "Running Emacs Unit Tests..."
	@.venv/bin/pytest --emacs || exit 1

test-backend:
	@echo "Running System Tests for backend: ${BACKEND}"
	@.venv/bin/pytest --backend=${BACKEND} --core --cli --server --emacs --system || exit 1

test-system:
	@echo "Running System Tests for all backends..."
	@.venv/bin/pytest --backend=all --core --cli --server --emacs --system || exit 1

