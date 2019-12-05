SHELL = /usr/bin/env bash

PROJECT    = ttc
CABAL_FILE = $(PROJECT).cabal
PACKAGE    = $(PROJECT)-haskell

ifneq ($(origin RESOLVER), undefined)
	RESOLVER_ARGS := "--resolver" "$(RESOLVER)"
endif

ifneq ($(origin CONFIG), undefined)
	STACK_YAML_ARGS := "--stack-yaml" "$(CONFIG)"
endif

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
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS)

clean:
	@stack clean

clean-all: clean
	@rm -rf .stack-work
	@rm -rf examples/.stack-work
	@rm -rf build
	@rm -f *.yaml.lock

coverage:
	@stack test --coverage $(RESOLVER_ARGS) $(STACK_YAML_ARGS)

doc-api:
	@stack haddock $(RESOLVER_ARGS) $(STACK_YAML_ARGS)

example-invalid:
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		--flag ttc-examples:example-invalid

example-mkvalid:
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		--flag ttc-examples:example-mkvalid
	@stack exec example-mkvalid

example-prompt:
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		--flag ttc-examples:example-prompt
	@stack exec example-prompt

example-uname:
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		--flag ttc-examples:example-uname
	@stack exec example-uname

example-valid:
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		--flag ttc-examples:example-valid
	@stack exec example-valid

examples:
	@stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		--flag ttc-examples:examples

grep:
	$(eval E := "")
	@test -n "$(E)" || $(call die,"usage: make grep E=expression")
	@$(call all_files) | xargs grep -Hn '$(E)' || true

help:
	@echo "make build            build package *"
	@echo "make clean            clean package"
	@echo "make clean-all        clean package and remove artifacts"
	@echo "make coverage         run tests with code coverage *"
	@echo "make doc-api          build API documentation *"
	@echo "make example-invalid  build demo-invalid, which should fail *"
	@echo "make example-mkvalid  build and run demo-mkvalid *"
	@echo "make example-prompt   build and run demo-prompt *"
	@echo "make example-uname    build and run demo-uname *"
	@echo "make example-valid    build and run demo-valid *"
	@echo "make examples         build all buldable demos *"
	@echo "make grep             grep all non-hidden files for expression E"
	@echo "make help             show this help"
	@echo "make hlint            run hlint on all Haskell source"
	@echo "make hsgrep           grep all Haskell source for expression E"
	@echo "make hsrecent         show N most recently modified Haskell files"
	@echo "make hssloc           count lines of Haskell source"
	@echo "make recent           show N most recently modified files"
	@echo "make repl             run REPL"
	@echo "make sdist            create source tarball for Hackage"
	@echo "make source-git       create source tarball of git TREE"
	@echo "make source-tar       create source tarball using tar"
	@echo "make test             run tests, optionally for pattern P *"
	@echo "make test-doc         run tests and build API documentation *"
	@echo "make todo             search for TODO items"
	@echo "make version          show current version"
	@echo
	@echo "* Use RESOLVER to specify a resolver."
	@echo "* Use CONFIG to specify a Stack configuration file."

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
	@stack exec ghci $(RESOLVER_ARGS) $(STACK_YAML_ARGS)

sdist:
	$(eval BRANCH := $(shell git rev-parse --abbrev-ref HEAD))
	@test "${BRANCH}" = "master" || $(call die,"not in master branch")
	@stack sdist

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

test:
	$(eval P := "")
	@test -z "$(P)" \
		&& stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
		|| stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
				--test-arguments '--pattern $(P)'

test-doc:
	@stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) --haddock --test

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
	help hlint hsgrep hsrecent hssloc recent repl sdist source-git source-tar \
	test test-doc todo version
