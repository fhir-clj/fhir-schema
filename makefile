.PHONY: install-hooks repl test clean build run format lint check fix check-json update-golden deps

all: format lint update-golden

install-hooks:
	@bash scripts/install-hooks.sh

deps:
	clj -P

repl:
	clj -M:dev -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"

check: test lint format-check

fix: format update-golden

###########################################################

test:
	clj -M:test

update-golden:
	UPDATE_GOLDEN=true clj -M:test

lint:
	clj -M:lint

format:
	clj -M:format fix

format-check:
	clj -M:format check

###########################################################

run:
	clj -M -m transpiler.core

build:
	clj -T:build uber

clean:
	rm -rf target .cpcache
