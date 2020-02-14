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

RESOLVER_ARGS :=
ifneq ($(origin RESOLVER), undefined)
  RESOLVER_ARGS := "--resolver" "$(RESOLVER)"
endif

STACK_YAML_ARGS :=
ifneq ($(origin CONFIG), undefined)
  STACK_YAML_ARGS := "--stack-yaml" "$(CONFIG)"
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

_default: build
.PHONY: _default

build:
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS)
.PHONY: build

clean:
> @stack clean
.PHONY: clean

clean-all: clean
> @rm -rf .stack-work
> @rm -rf examples/.stack-work
> @rm -rf build
> @rm -f *.yaml.lock
.PHONY: clean-all

coverage:
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @stack test --coverage $(RESOLVER_ARGS) $(STACK_YAML_ARGS)
.PHONY: coverage

doc-api:
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @stack haddock $(RESOLVER_ARGS) $(STACK_YAML_ARGS)
.PHONY: doc-api

example-enum:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-enum
> @stack exec example-enum
.PHONY: example-enum

example-invalid:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-invalid
.PHONY: example-invalid

example-lift:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-lift
> @stack exec example-lift
.PHONY: example-lift

example-mkvalid:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-mkvalid
> @stack exec example-mkvalid
.PHONY: example-mkvalid

example-mkuvalid:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-mkuvalid
> @stack exec example-mkuvalid
.PHONY: example-mkuvalid

example-prompt:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-prompt
> @stack exec example-prompt
.PHONY: example-prompt

example-uname:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-uname
> @stack exec example-uname
.PHONY: example-uname

example-uvalidof:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-uvalidof
> @stack exec example-uvalidof
.PHONY: example-uvalidof

example-uvalidqq:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-uvalidqq
> @stack exec example-uvalidqq
.PHONY: example-uvalidqq

example-valid:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-valid
> @stack exec example-valid
.PHONY: example-valid

example-validof:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:example-validof
> @stack exec example-validof
.PHONY: example-validof

examples:
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --flag ttc-examples:examples
.PHONY: examples

grep:
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help:
> @echo "make build             build package *"
> @echo "make clean             clean package"
> @echo "make clean-all         clean package and remove artifacts"
> @echo "make coverage          run tests with code coverage *"
> @echo "make doc-api           build API documentation *"
> @echo "make example-enum      build and run example-enum*"
> @echo "make example-invalid   build example-invalid, which should fail *"
> @echo "make example-lift      build and run example-lift*"
> @echo "make example-mkvalid   build and run example-mkvalid *"
> @echo "make example-mkuvalid  build and run example-mkuvalid *"
> @echo "make example-prompt    build and run example-prompt *"
> @echo "make example-uname     build and run example-uname *"
> @echo "make example-uvalidof  build and run example-uvalidof *"
> @echo "make example-uvalidqq  build and run example-uvalidqq *"
> @echo "make example-valid     build and run example-valid *"
> @echo "make example-validof   build and run example-validof *"
> @echo "make examples          build all buldable examples *"
> @echo "make grep              grep all non-hidden files for expression E"
> @echo "make help              show this help"
> @echo "make hlint             run hlint on all Haskell source"
> @echo "make hsgrep            grep all Haskell source for expression E"
> @echo "make hsrecent          show N most recently modified Haskell files"
> @echo "make hssloc            count lines of Haskell source"
> @echo "make recent            show N most recently modified files"
> @echo "make repl              enter a REPL *"
> @echo "make sdist             create source tarball for Hackage"
> @echo "make source-git        create source tarball of git TREE"
> @echo "make source-tar        create source tarball using tar"
> @echo "make test              run tests, optionally for pattern P *"
> @echo "make test-doc          run tests and build API documentation *"
> @echo "make todo              search for TODO items"
> @echo "make version           show current version"
> @echo
> @echo "* Use RESOLVER to specify a resolver."
> @echo "* Use CONFIG to specify a Stack configuration file."
.PHONY: help

hlint:
> @$(call hs_files) | xargs hlint -i "Parse error"
.PHONY: hlint

hsgrep:
> $(eval E := "")
> @test -n "$(E)" || $(call die,"usage: make hsgrep E=expression")
> @$(call hs_files) | xargs grep -Hn '$(E)' || true
.PHONY: hsgrep

hsrecent:
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -name '*.hs' -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: hsrecent

hssloc:
> @$(call hs_files) | xargs wc -l | tail -n 1 | sed 's/^ *\([0-9]*\).*$$/\1/'
.PHONY: hssloc

recent:
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

repl:
> @stack exec ghci $(RESOLVER_ARGS) $(STACK_YAML_ARGS)
.PHONY: repl

sdist:
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref HEAD))
> @test "${BRANCH}" = "master" || $(call die,"not in master branch")
> @stack sdist
.PHONY: sdist

source-git:
> $(eval TREE := "HEAD")
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref $(TREE)))
> @test "${BRANCH}" = "master" || echo "WARNING: Not in master branch!" >&2
> $(eval VERSION := $(shell \
    grep '^version:' $(CABAL_FILE) | sed 's/^version: *//'))
> @mkdir -p build
> @git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ $(TREE) \
>   | xz \
>   > build/$(PROJECT)-$(VERSION).tar.xz
.PHONY: source-git

source-tar:
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

test:
> $(eval P := "")
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @test -z "$(P)" \
>   && stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   || stack test $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>       --test-arguments '--pattern $(P)'
.PHONY: test

test-doc:
> @command -v hr >/dev/null 2>&1 && hr -t || true
> @stack build $(RESOLVER_ARGS) $(STACK_YAML_ARGS) \
>   --haddock --test --bench --no-run-benchmarks
.PHONY: test-doc

todo:
> @find . -type f \
>   -not -path '*/\.*' \
>   -not -path './build/*' \
>   -not -path './project/*' \
>   -not -path ./Makefile \
>   | xargs grep -Hn TODO || true
.PHONY: todo

version:
> @grep '^version:' $(CABAL_FILE) | sed 's/^version: */$(PROJECT) /'
> @grep '^version:' "examples/ttc-examples.cabal" \
>   | sed 's/^version: */ttc-examples /'
.PHONY: version
