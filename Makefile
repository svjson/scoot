.PHONY: dev-setup test-backend test-core test-system test-emacs test-emacs-unit test
BACKEND ?= all

dev-setup:
	python -m venv .venv
	.venv/bin/pip install -e ./core
	.venv/bin/pip install -e ./cli
	.venv/bin/pip install -e ./server
	.venv/bin/pip install -r requirements-test.txt

test: test-system

test-core:
	@echo "Running Core Tests..."
	@.venv/bin/pytest core --core || exit 1

test-emacs:
	@echo "Running Emacs Integration Tests for backend: ${BACKEND}"
	@.venv/bin/pytest . --backend=${BACKEND} --emacs --emacs-unit || exit 1

test-emacs-unit:
	@echo "Running Emacs Unit Tests..."
	@.venv/bin/pytest . --emacs-unit || exit 1

test-backend:
	@echo "Running System Tests for backend: ${BACKEND}"
	@.venv/bin/pytest . --backend=${BACKEND} --core --cli --emacs || exit 1

test-system:
	@echo "Running System Tests for all backends..."
	@.venv/bin/pytest . --backend=${BACKEND} --core --cli --emacs --emacs-unit || exit 1

