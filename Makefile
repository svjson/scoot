.PHONY: dev-setup

dev-setup:
	python -m venv .venv
	.venv/bin/pip install -e ./core
	.venv/bin/pip install -e ./cli
	.venv/bin/pip install -e ./server
