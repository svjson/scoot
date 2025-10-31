.PHONY: dev-setup test-backend test-core test-system test
BACKEND ?= all

dev-setup:
	python -m venv .venv
	.venv/bin/pip install -e ./core
	.venv/bin/pip install -e ./cli
	.venv/bin/pip install -e ./server

test: test-core test-system

test-core:
	@echo "Running Core Tests..."
	@.venv/bin/pytest core || exit 1

test-backend:
	@echo "Running System Tests for backend: ${BACKEND}"
	@.venv/bin/pytest . --backend=${BACKEND} --cli --emacs || exit 1

test-system:
	@echo "Running System Tests for all backends..."
	@.venv/bin/pytest . --backend=all --cli --emacs || exit 1

