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

ifneq ($(origin CABAL), undefined)
  MODE := cabal
  CABAL_ARGS :=
  ifneq ($(origin PROJECT_FILE), undefined)
    CABAL_ARGS += "--project-file=$(PROJECT_FILE)"
  else
    PROJECT_FILE := cabal-$(shell ghc --version | sed 's/.* //').project
    ifneq (,$(wildcard $(PROJECT_FILE)))
      CABAL_ARGS += "--project-file=$(PROJECT_FILE)"
    endif
  endif
else
  MODE := stack
  STACK_ARGS :=
  ifneq ($(origin CONFIG), undefined)
    STACK_ARGS += --stack-yaml "$(CONFIG)"
  endif
  ifneq ($(origin RESOLVER), undefined)
    STACK_ARGS += --resolver "$(RESOLVER)"
  endif
  ifneq ($(origin STACK_NIX_PATH), undefined)
    STACK_ARGS += "--nix-path=$(STACK_NIX_PATH)"
  endif
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

build: hr
build: # build package *
ifeq ($(MODE), cabal)
> cabal v2-build $(CABAL_ARGS)
else
> stack build $(STACK_ARGS)
endif
.PHONY: build

clean: # clean package
ifeq ($(MODE), cabal)
> @rm -rf dist-newstyle
else
> @stack clean
endif
.PHONY: clean

clean-all: clean
clean-all: # clean package and remove artifacts
> @rm -rf .hie
> @rm -rf .stack-work examples/.stack-work
> @rm -rf build
> @rm -rf dist-newstyle
> @rm -f *.yaml.lock
> @rm -f cabal.project.local
> @rm -f result*
.PHONY: clean-all

coverage: hr
coverage: # run tests with code coverage *
ifeq ($(MODE), cabal)
> cabal v2-test --enable-coverage --enable-tests --test-show-details=always \
>   $(CABAL_ARGS)
else
> stack test --coverage $(STACK_ARGS)
> stack hpc report .
endif
.PHONY: coverage

doc-api: hr
doc-api: # build API documentation *
ifeq ($(MODE), cabal)
> cabal v2-haddock $(CABAL_ARGS)
else
> stack haddock $(STACK_ARGS)
endif
.PHONY: doc-api

example-enum: hr
example-enum: # build and run example-enum *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-enum
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-enum
> stack exec example-enum
endif
.PHONY: example-enum

example-invalid: hr
example-invalid: # build example-invalid, which should fail *
ifeq ($(MODE), cabal)
> cabal v2-build $(CABAL_ARGS) ttc-examples -f example-invalid
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-invalid
endif
.PHONY: example-invalid

example-lift: hr
example-lift: # build and run example-lift *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-lift
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-lift
> stack exec example-lift
endif
.PHONY: example-lift

example-mkvalid: hr
example-mkvalid: # build and run example-mkvalid *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-mkvalid
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-mkvalid
> stack exec example-mkvalid
endif
.PHONY: example-mkvalid

example-mkuvalid: hr
example-mkuvalid: # build and run example-mkuvalid *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-mkuvalid
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-mkuvalid
> stack exec example-mkuvalid
endif
.PHONY: example-mkuvalid

example-prompt: hr
example-prompt: # build and run example-prompt *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-prompt
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-prompt
> stack exec example-prompt
endif
.PHONY: example-prompt

example-uname: hr
example-uname: # build and run example-uname *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-uname
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-uname
> stack exec example-uname
endif
.PHONY: example-uname

example-uvalidof: hr
example-uvalidof: # build and run example-uvalidof *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-uvalidof
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-uvalidof
> stack exec example-uvalidof
endif
.PHONY: example-uvalidof

example-uvalidqq: hr
example-uvalidqq: # build and run example-uvalidqq *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-uvalidqq
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-uvalidqq
> stack exec example-uvalidqq
endif
.PHONY: example-uvalidqq

example-valid: hr
example-valid: # build and run example-valid *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-valid
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-valid
> stack exec example-valid
endif
.PHONY: example-valid

example-validof: hr
example-validof: # build and run example-validof *
ifeq ($(MODE), cabal)
> cabal v2-run $(CABAL_ARGS) example-validof
else
> stack build $(STACK_ARGS) --flag ttc-examples:example-validof
> stack exec example-validof
endif
.PHONY: example-validof

examples: hr
examples: # build all buldable examples *
ifeq ($(MODE), cabal)
> cabal v2-build $(CABAL_ARGS) ttc-examples -f examples
else
> stack build $(STACK_ARGS) --flag ttc-examples:examples
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
> @echo "* Set CABAL to use Cabal instead of Stack."
> @echo "* Set CONFIG to specify a Stack configuration file."
> @echo "* Set PROJECT_FILE to specify a cabal.project file."
> @echo "* Set RESOLVER to specify a Stack resolver."
> @echo "* Set STACK_NIX_PATH to specify a Stack Nix path."
.PHONY: help

hlint: # run hlint on all Haskell source
> @$(call hs_files) | xargs hlint
.PHONY: hlint

hr: #internal# display a horizontal rule
> @command -v hr >/dev/null 2>&1 && hr -t || true
.PHONY: hr

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
> cabal repl $(CABAL_ARGS)
else
> stack exec ghci $(STACK_ARGS)
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

stan: hr
stan: export STAN_USE_DEFAULT_CONFIG=True
stan: # run stan static analysis
ifeq ($(MODE), cabal)
> @cabal v2-build -f write-hie
else
> @stack build --flag $(PACKAGE):write-hie
endif
> @stan
.PHONY: stan

test: hr
test: # run tests, optionally for pattern P *
> $(eval P := "")
ifeq ($(MODE), cabal)
> @test -z "$(P)" \
>   && cabal v2-test --enable-tests --test-show-details=always \
>       $(CABAL_ARGS) \
>   || cabal v2-test --enable-tests --test-show-details=always \
>       --test-option '--pattern=$(P)' $(CABAL_ARGS)
else
> @test -z "$(P)" \
>   && stack test $(STACK_ARGS) \
>   || stack test $(STACK_ARGS) --test-arguments '--pattern $(P)'
endif
.PHONY: test

test-all: # run tests and build examples for all configured Stackage releases
ifeq ($(MODE), cabal)
> $(call die,"test-all not supported in CABAL mode")
endif
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
> @command -v hr >/dev/null 2>&1 && hr "stack-8.10.7.yaml" || true
> @make test-doc CONFIG=stack-8.10.7.yaml
> @make examples CONFIG=stack-8.10.7.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-9.0.1.yaml" || true
> @make test-doc CONFIG=stack-9.0.1.yaml
> @make examples CONFIG=stack-9.0.1.yaml
> @command -v hr >/dev/null 2>&1 && hr "stack-9.2.1.yaml" || true
> @make test-doc CONFIG=stack-9.2.1.yaml
> @make examples CONFIG=stack-9.2.1.yaml
.PHONY: test-all

test-doc: hr
test-doc: # run tests and build API documentation *
ifeq ($(MODE), cabal)
> @cabal v2-test --enable-tests --test-show-details=always $(CABAL_ARGS)
> @cabal v2-haddock $(CABAL_ARGS)
else
> @stack build $(STACK_ARGS) --haddock --test --bench --no-run-benchmarks
endif
.PHONY: test-doc

test-nightly: # run tests for the latest Stackage nightly release
ifeq ($(MODE), cabal)
> $(call die,"test-nightly not supported in CABAL mode")
endif
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
