SHELL = /usr/bin/env bash

PROJECT    = ttc
CABAL_FILE = $(PROJECT).cabal
PACKAGE    = $(PROJECT)-haskell

define all_files
	find . -not -path '*/\.*' -type f
endef

define die
	(echo "error: $(1)" ; false)
endef

define hs_files
	find . -not -path '*/\.*' -type f -name '*.hs'
endef

_default: build

build:
	@stack build

clean:
	@stack clean

clean-all: clean
	@rm -rf .stack-work
	@rm -rf build
	@rm -f *.yaml.lock

coverage:
	@stack test --coverage

doc-api:
	@stack haddock

example-invalid:
	@stack build --flag ttc:example-invalid

example-mkvalid:
	@stack build --flag ttc:example-mkvalid
	@stack exec example-mkvalid

example-prompt:
	@stack build --flag ttc:example-prompt
	@stack exec example-prompt

example-uname:
	@stack build --flag ttc:example-uname
	@stack exec example-uname

example-valid:
	@stack build --flag ttc:example-valid
	@stack exec example-valid

examples:
	@stack build --flag ttc:examples

grep:
	$(eval E := "")
	@test -n "$(E)" || $(call die,"usage: make grep E=expression")
	@$(call all_files) | xargs grep -Hn '$(E)' || true

help:
	@echo "make build            build package"
	@echo "make clean            clean package"
	@echo "make clean-all        clean package and remove artifacts"
	@echo "make coverage         run tests with code coverage"
	@echo "make doc-api          build API documentation"
	@echo "make example-invalid  build demo-invalid, which should fail"
	@echo "make example-mkvalid  build and run demo-mkvalid"
	@echo "make example-prompt   build and run demo-prompt"
	@echo "make example-uname    build and run demo-uname"
	@echo "make example-valid    build and run demo-valid"
	@echo "make examples         build all buldable demos"
	@echo "make grep             grep all non-hidden files for expression E"
	@echo "make help             show this help"
	@echo "make hlint            run hlint on all Haskell source"
	@echo "make hsgrep           grep all Haskell source for expression E"
	@echo "make hsrecent         show N most recently modified Haskell files"
	@echo "make hssloc           count lines of Haskell source"
	@echo "make recent           show N most recently modified files"
	@echo "make repl             run REPL"
	@echo "make source-git       create source tarball of git TREE"
	@echo "make source-tar       create source tarball using tar"
	@echo "make stack            build and test using file F or resolver R"
	@echo "make test             run tests, optionally for pattern P"
	@echo "make todo             search for TODO items"
	@echo "make version          show current version"

hlint:
	@$(call hs_files) | xargs hlint -i "Parse error"

hsgrep:
	@$(eval E := "")
	@test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
	@$(call hs_files) | xargs grep -Hn '$(E)' || true

hsrecent:
	$(eval N := "10")
	@find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
		| sort --reverse \
		| head -n $(N)

hssloc:
	@$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'

recent:
	$(eval N := "10")
	@find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
		| sort --reverse \
		| head -n $(N)

repl:
	@stack exec ghci

source-git:
	$(eval BRANCH := $(shell git rev-parse --abbrev-ref HEAD))
	@test "${BRANCH}" = "master" || echo "WARNING: Not in master branch!" >&2
	$(eval TREE := "HEAD")
	$(eval VERSION := $(shell \
		grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
	@mkdir -p build
	@git archive --format=tar --prefix=$(PACKAGE)-$(VERSION)/ $(TREE) \
		| xz \
		> build/$(PACKAGE)-$(VERSION).tar.xz

source-tar:
	$(eval VERSION := $(shell \
		grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
	@mkdir -p build
	@sed -e 's,^/,./,' -e 's,/$$,,' .gitignore > build/.gitignore
	@tar \
		--exclude-vcs \
		--exclude-ignore-recursive=build/.gitignore \
		--transform "s,^\.,$(PACKAGE)-$(VERSION)," \
		-Jcf build/$(PACKAGE)-$(VERSION).tar.xz \
		.
	@rm -f build/.gitignore

stack:
	$(eval F := "")
	$(eval R := "nightly")
	@test -z "$(F)" \
		&& stack build --resolver $(R) --haddock --test \
		|| stack build --stack-yaml $(F) --haddock --test

test:
	$(eval P := "")
	@test -z "$(P)" \
		&& stack test \
		|| stack test --test-arguments '--pattern $(P)'

todo:
	@find . -type f \
		-not -path '*/\.*' \
		-not -path './build/*' \
		-not -path './project/*' \
		-not -path ./Makefile \
		| xargs grep -Hn TODO || true

version:
	$(eval VERSION := $(shell \
		grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
	@echo $(VERSION)

.PHONY: _default build clean clean-all coverage doc-api example-invalid \
	example-mkvalid example-prompt example-uname example-valid examples grep \
	help hlint hsgrep hsrecent hssloc recent repl source-git source-tar stack \
	test todo version
