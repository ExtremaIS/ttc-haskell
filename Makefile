##############################################################################
# Project configuration

PACKAGE    := ttc
CABAL_FILE := $(PACKAGE).cabal
PROJECT    := $(PACKAGE)-haskell

##############################################################################
# Make configuration

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error GNU Make 4.0 or later required)
endif
.RECIPEPREFIX := >

SHELL := bash
.SHELLFLAGS := -o nounset -o errexit -o pipefail -c

MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --warn-undefined-variables

.DEFAULT_GOAL := build

NIX_PATH_ARGS :=
ifneq ($(origin STACK_NIX_PATH), undefined)
  NIX_PATH_ARGS := "--nix-path=$(STACK_NIX_PATH)"
endif

RESOLVER_ARGS :=
ifneq ($(origin RESOLVER), undefined)
  RESOLVER_ARGS := "--resolver" "$(RESOLVER)"
endif

STACK_YAML_ARGS :=
ifneq ($(origin CONFIG), undefined)
  STACK_YAML_ARGS := "--stack-yaml" "$(CONFIG)"
endif

MODE := stack
ifneq ($(origin CABAL), undefined)
  MODE := cabal
endif

##############################################################################
# Functions

define all_files
  find . -not -path '*/\.*' -type f
endef

define die
  (echo "error: $(1)" ; false)
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

##############################################################################
# Rules

build: # build package *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-build
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
endif
.PHONY: build

clean: # clean package
ifeq ($(MODE), cabal)
> @rm -rf dist-newstyle
else
> @stack clean
endif
.PHONY: clean

clean-all: clean # clean package and remove artifacts
> @rm -rf .hie
> @rm -rf .stack-work
> @rm -rf examples/.stack-work
> @rm -rf build
> @rm -rf dist-newstyle
> @rm -f *.yaml.lock
.PHONY: clean-all

coverage: # run tests with code coverage *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-test --enable-coverage --enable-tests --test-show-details=always
else
> @stack test --coverage $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
> @stack hpc report .
endif
.PHONY: coverage

doc-api: # build API documentation *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-haddock
else
> @stack haddock $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
endif
.PHONY: doc-api

example-enum: # build and run example-enum *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-enum
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-enum
> @stack exec example-enum
endif
.PHONY: example-enum

example-invalid: # build example-invalid, which should fail *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-build ttc-examples -f example-invalid
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-invalid
endif
.PHONY: example-invalid

example-lift: # build and run example-lift *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-lift
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-lift
> @stack exec example-lift
endif
.PHONY: example-lift

example-mkvalid: # build and run example-mkvalid *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-mkvalid
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-mkvalid
> @stack exec example-mkvalid
endif
.PHONY: example-mkvalid

example-mkuvalid: # build and run example-mkuvalid *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-mkuvalid
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-mkuvalid
> @stack exec example-mkuvalid
endif
.PHONY: example-mkuvalid

example-prompt: # build and run example-prompt *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-prompt
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-prompt
> @stack exec example-prompt
endif
.PHONY: example-prompt

example-uname: # build and run example-uname *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-uname
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-uname
> @stack exec example-uname
endif
.PHONY: example-uname

example-uvalidof: # build and run example-uvalidof *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-uvalidof
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-uvalidof
> @stack exec example-uvalidof
endif
.PHONY: example-uvalidof

example-uvalidqq: # build and run example-uvalidqq *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-uvalidqq
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-uvalidqq
> @stack exec example-uvalidqq
endif
.PHONY: example-uvalidqq

example-valid: # build and run example-valid *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-valid
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-valid
> @stack exec example-valid
endif
.PHONY: example-valid

example-validof: # build and run example-validof *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-run example-validof
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:example-validof
> @stack exec example-validof
endif
.PHONY: example-validof

examples: # build all buldable examples *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-build ttc-examples -f examples
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --flag ttc-examples:examples
endif
.PHONY: examples

grep: # grep all non-hidden files for expression E
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help: # show this help
> @grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>   | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>   | column -t -s $$'\t'
> @echo
> @echo "* Use STACK_NIX_PATH to specify a Nix path."
> @echo "* Use RESOLVER to specify a resolver."
> @echo "* Use CONFIG to specify a Stack configuration file."
> @echo "* Use CABAL to use Cabal instead of Stack."
.PHONY: help

hlint: # run hlint on all Haskell source
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hsgrep: # grep all Haskell source for expression E
> $(eval E := "")
> @test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
> @$(call hs_files) | xargs grep -Hn '$(E)' || true
.PHONY: hsgrep

hsrecent: # show N most recently modified Haskell files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: hsrecent

hssloc: # count lines of Haskell source
> @$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'
.PHONY: hssloc

recent: # show N most recently modified files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

repl: # enter a REPL *
ifeq ($(MODE), cabal)
> @cabal repl
else
> @stack exec ghci $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS)
endif
.PHONY: repl

sdist: # create source tarball for Hackage
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref HEAD))
> @test "${BRANCH}" = "main" || $(call die,"not in main branch")
ifeq ($(MODE), cabal)
> @cabal sdist
else
> @stack sdist
endif
.PHONY: sdist

source-git: # create source tarball of git TREE
> $(eval TREE := "HEAD")
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref $(TREE)))
> @test "$(BRANCH)" = "main" || echo "WARNING: Not in main branch!" >&2
> $(eval DIRTY := $(shell git diff --shortstat | wc -l))
> @test "$(DIRTY)" = "0" \
>   || echo "WARNING: Not including non-committed changes!" >&2
> $(eval UNTRACKED := $(shell \
    git ls-files --other --directory --no-empty-directory --exclude-standard \
    | wc -l))
> @test "$(UNTRACKED)" = "0" \
>   || echo "WARNING: Not including untracked files!" >&2
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ $(TREE) \
>   | xz \
>   > build/$(PROJECT)-$(VERSION).tar.xz
.PHONY: source-git

source-tar: # create source tarball using tar
> $(eval DIRTY := $(shell git diff --shortstat | wc -l))
> @test "$(DIRTY)" = "0" \
>   || echo "WARNING: Including non-committed changes!" >&2
> $(eval UNTRACKED := $(shell \
    git ls-files --other --directory --no-empty-directory --exclude-standard \
    | wc -l))
> @test "$(UNTRACKED)" = "0" \
>   || echo "WARNING: Including untracked files!" >&2
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @sed -e 's,^/,./,' -e 's,/$$,,' .gitignore > build/.gitignore
> @tar \
>   --exclude-vcs \
>   --exclude-ignore-recursive=build/.gitignore \
>   --transform "s,^\.,$(PROJECT)-$(VERSION)," \
>   -Jcf build/$(PROJECT)-$(VERSION).tar.xz \
>   .
> @rm -f build/.gitignore
.PHONY: source-tar

stan: # run stan static analysis
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-build -f write-hie
else
> @stack build --flag $(PACKAGE):write-hie
endif
> @stan
.PHONY: stan

test: # run tests, optionally for pattern P *
> $(eval P := "")
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @test -z "$(P)" \
>   && cabal v2-test --enable-tests --test-show-details=always \
>   || cabal v2-test --enable-tests --test-show-details=always \
>       --test-option '--pattern=$(P)'
else
> @test -z "$(P)" \
>   && stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   || stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>       --test-arguments '--pattern $(P)'
endif
.PHONY: test

test-all: # run tests and build examples for all configured Stackage releases
> @command -v hr >/dev/null 2>&1 && hr "stack-8.2.2.yaml" || true
> @make test-doc CONFIG=stack-8.2.2.yaml
> @make examples CONFIG=stack-8.2.2.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.4.4.yaml" || true
> @make test-doc CONFIG=stack-8.4.4.yaml
> @make examples CONFIG=stack-8.4.4.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.6.5.yaml" || true
> @make test-doc CONFIG=stack-8.6.5.yaml
> @make examples CONFIG=stack-8.6.5.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.8.4.yaml" || true
> @make test-doc CONFIG=stack-8.8.4.yaml
> @make examples CONFIG=stack-8.8.4.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-8.10.4.yaml" || true
> @make test-doc CONFIG=stack-8.10.4.yaml
> @make examples CONFIG=stack-8.10.4.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-9.0.1.yaml" || true
> @make test-doc CONFIG=stack-9.0.1.yaml
> @make examples CONFIG=stack-9.0.1.yaml
.PHONY: test-all

test-doc: # run tests and build API documentation *
> @command -v hr >/dev/null 2>&1 && hr -t || true
ifeq ($(MODE), cabal)
> @cabal v2-test --enable-tests --test-show-details=always
> @cabal v2-haddock
else
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) $(NIX_PATH_ARGS) \
>   --haddock --test --bench --no-run-benchmarks
endif
.PHONY: test-doc

test-nightly: # run tests for the latest Stackage nightly release
> @command -v hr >/dev/null 2>&1 && hr nightly || true
> @make test RESOLVER=nightly
.PHONY: test-nightly

todo: # search for TODO items
> @find . -type f \
>   -not -path '*/\.*' \
>   -not -path './build/*' \
>   -not -path './project/*' \
>   -not -path ./Makefile \
>   | xargs grep -Hn TODO \
>   | grep -v '^Binary file ' \
>   || true
.PHONY: todo

version: # show current version
> @grep '^version:' $(CABAL_FILE) | sed 's/^version: */$(PROJECT) /'
> @grep '^version:' "examples/ttc-examples.cabal" \
>   | sed 's/^version: */ttc-examples /'
.PHONY: version
