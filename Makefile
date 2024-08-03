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

MODE ?= cabal

ifeq ($(MODE), cabal)
  GHC_VERSION ?= $(shell ghc --version | sed 's/.* //')
  CABAL_ARGS := --with-ghc ghc-$(GHC_VERSION)
  ifneq ($(origin PROJECT_FILE), undefined)
    CABAL_ARGS += "--project-file=$(PROJECT_FILE)"
  else
    PROJECT_FILE_AUTO := cabal-$(GHC_VERSION).project
    ifneq (,$(wildcard $(PROJECT_FILE_AUTO)))
      CABAL_ARGS += "--project-file=$(PROJECT_FILE_AUTO)"
    endif
  endif
  ifneq ($(origin CONFIG), undefined)
    $(error CONFIG set in cabal MODE)
  endif
  ifneq ($(origin RESOLVER), undefined)
    $(error RESOLVER set in cabal MODE)
  endif
else ifeq ($(MODE), stack)
  STACK_ARGS :=
  ifneq ($(origin CONFIG), undefined)
    STACK_ARGS += --stack-yaml "$(CONFIG)"
  endif
  ifneq ($(origin RESOLVER), undefined)
    STACK_ARGS += --resolver "$(RESOLVER)"
  endif
  ifneq ($(origin GHC_VERSION), undefined)
    $(error GHC_VERSION set in stack MODE)
  endif
  ifneq ($(origin CABAL_ARGS), undefined)
    $(error CABAL_ARGS set in stack MODE)
  endif
else
  $(error unknown MODE: $(MODE))
endif

##############################################################################
# Functions

define all_files
  find . -not -path '*/\.*' -type f
endef

define die
  (echo "error: $(1)" ; false)
endef

define get_version
$(shell grep '^version:' $(1) | sed 's/^version: *//')
endef

define hs_files
  find . -not -path '*/\.*' -type f -name '*.hs'
endef

##############################################################################
# Rules

build: hr
build: # build package *
ifeq ($(MODE), stack)
> stack build $(STACK_ARGS) --test --bench --no-run-tests --no-run-benchmarks
else
> cabal build all $(CABAL_ARGS) --enable-tests --enable-benchmarks
endif
.PHONY: build

clean: # clean packages
> @rm -rf stack*.yaml.lock
> @rm -rf .stack-work ttc/.stack-work ttc-examples/.stack-work
> @rm -f cabal*.project.local
> @rm -rf dist-newstyle
.PHONY: clean

clean-all: clean
clean-all: #internal# clean package and remove build artifacts
#> @rm -rf build
.PHONY: clean-all

coverage: hr
coverage: # run tests with code coverage *
ifeq ($(MODE), stack)
> stack test $(STACK_ARGS) --coverage
else
> cabal test all $(CABAL_ARGS) \
>   --enable-coverage --enable-tests --test-show-details=always
endif
.PHONY: coverage

doc-api: hr
doc-api: # build API documentation *
ifeq ($(MODE), stack)
> stack haddock $(STACK_ARGS)
else
> cabal haddock all $(CABAL_ARGS)
endif
.PHONY: doc-api

grep: # grep all non-hidden files for expression E
> $(eval E:= "")
> @test -n "$(E)" || $(call die,"usage: make grep E=expression")
> @$(call all_files) | xargs grep -Hn '$(E)' || true
.PHONY: grep

help: # show this help
> @if command -v column >/dev/null 2>&1 \
>  ; then \
>    grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>    | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>    | column -t -s $$'\t' \
>  ; else \
>    grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
>    | sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
>  ; fi
> @echo
> @echo "Cabal mode (MODE=cabal)"
> @echo "  * Set GHC_VERSION to specify a GHC version."
> @echo "  * Set PROJECT_FILE to specify a cabal.project file."
> @echo
> @echo "Stack mode (MODE=stack)"
> @echo "  * Set CONFIG to specify a stack.yaml file."
> @echo "  * Set RESOLVER to specify a Stack resolver."
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

ignored: # list files ignored by git
> @git ls-files . --ignored --exclude-standard --others
.PHONY: ignored

recent: # show N most recently modified files
> $(eval N := "10")
> @find . -not -path '*/\.*' -type f -printf '%T+ %p\n' \
>   | sort --reverse \
>   | head -n $(N)
.PHONY: recent

test: hr
test: # run tests, optionally for pattern P *
> $(eval P := "")
ifeq ($(MODE), stack)
> @test -z "$(P)" \
>   && stack test $(STACK_ARGS) \
>   || stack test $(STACK_ARGS) --test-arguments '--pattern $(P)'
else
> @test -z "$(P)" \
>   && cabal test all $(CABAL_ARGS) \
>        --enable-tests --test-show-details=always \
>   || cabal test all $(CABAL_ARGS) \
>        --enable-tests --test-show-details=always \
>        --test-option '--pattern=$(P)'
endif
.PHONY: test

test-all: # run all configured tests and build examples using MODE
ifeq ($(MODE), stack)
> @make test-build CONFIG=stack-8.6.5.yaml
> @make test-build CONFIG=stack-8.8.4.yaml
> @make test-build CONFIG=stack-8.10.7.yaml
> @make test-build CONFIG=stack-9.0.2.yaml
> @make test-build CONFIG=stack-9.2.8.yaml
> @make test-build CONFIG=stack-9.4.8.yaml
> @make test-build CONFIG=stack-9.6.6.yaml
> @make test-build CONFIG=stack-9.8.2.yaml
> @make test-build CONFIG=stack-9.10.1.yaml
else
> @make test-build GHC_VERSION=8.6.5
> @make test-build GHC_VERSION=8.8.4
> @make test-build GHC_VERSION=8.10.7
> @make test-build GHC_VERSION=9.0.2
> @make test-build GHC_VERSION=9.2.8
> @make test-build GHC_VERSION=9.4.8
> @make test-build GHC_VERSION=9.6.6
> @make test-build GHC_VERSION=9.8.2
> @make test-build GHC_VERSION=9.10.1
endif
.PHONY: test-all

test-bounds-lower: ttc-test-bounds-lower
test-bounds-lower: # test lower bounds (Cabal only)
.PHONY: test-bounds-lower

test-bounds-upper: ttc-test-bounds-upper
test-bounds-upper: # test upper bounds (Cabal only)
.PHONY: test-bounds-upper

test-build: hr
test-build: build
test-build: test
test-build: doc-api
test-build: # build, run tests, build API documentation *
.PHONY: test-build

test-nightly: # run tests for the latest Stackage nightly release (Stack only)
> @make test MODE=stack RESOLVER=nightly
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

ttc: hr
ttc: # build ttc package *
ifeq ($(MODE), stack)
> stack build ttc $(STACK_ARGS) \
>   --test --bench --no-run-tests --no-run-benchmarks
else
> cabal build ttc $(CABAL_ARGS) --enable-tests --enable-benchmarks
endif
.PHONY: ttc

ttc-coverage: hr
ttc-coverage: # ttc: run tests with code coverage *
ifeq ($(MODE), stack)
> stack test ttc $(STACK_ARGS) --coverage
else
> cabal test ttc $(CABAL_ARGS) \
>   --enable-coverage --enable-tests --test-show-details=always
endif
.PHONY: ttc-coverage

ttc-doc-api: hr
ttc-doc-api: # ttc: build API documentation *
ifeq ($(MODE), stack)
> stack haddock ttc $(STACK_ARGS)
else
> cabal haddock ttc $(CABAL_ARGS)
endif
.PHONY: ttc-doc-api

ttc-examples: hr
ttc-examples: # build all buildable ttc-examples *
ifeq ($(MODE), stack)
> stack build ttc-examples $(STACK_ARGS)
else
> cabal build ttc-examples $(CABAL_ARGS)
endif
.PHONY: ttc-examples

ttc-example-enum: hr
ttc-example-enum: # build and run ttc-example-enum *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-enum
else
> cabal run $(CABAL_ARGS) ttc-example-enum
endif
.PHONY: ttc-example-enum

ttc-example-invalid: hr
ttc-example-invalid: # build ttc-example-invalid, which should fail *
ifeq ($(MODE), stack)
> stack build ttc-examples $(STACK_ARGS) \
>   --flag ttc-examples:ttc-example-invalid
else
> cabal build ttc-examples $(CABAL_ARGS) -f ttc-example-invalid
endif
.PHONY: ttc-example-invalid

ttc-example-lift: hr
ttc-example-lift: # build and run ttc-example-lift *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-lift
else
> cabal run $(CABAL_ARGS) ttc-example-lift
endif
.PHONY: ttc-example-lift

ttc-example-mkvalid: hr
ttc-example-mkvalid: # build and run ttc-example-mkvalid *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-mkvalid
else
> cabal run $(CABAL_ARGS) ttc-example-mkvalid
endif
.PHONY: ttc-example-mkvalid

ttc-example-mkuvalid: hr
ttc-example-mkuvalid: # build and run ttc-example-mkuvalid *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-mkuvalid
else
> cabal run $(CABAL_ARGS) ttc-example-mkuvalid
endif
.PHONY: ttc-example-mkuvalid

ttc-example-prompt: hr
ttc-example-prompt: # build and run ttc-example-prompt *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-prompt
else
> cabal run $(CABAL_ARGS) ttc-example-prompt
endif
.PHONY: ttc-example-prompt

ttc-example-uname: hr
ttc-example-uname: # build and run ttc-example-uname *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-uname
else
> cabal run $(CABAL_ARGS) ttc-example-uname
endif
.PHONY: ttc-example-uname

ttc-example-uvalidof: hr
ttc-example-uvalidof: # build and run ttc-example-uvalidof *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-uvalidof
else
> cabal run $(CABAL_ARGS) ttc-example-uvalidof
endif
.PHONY: ttc-example-uvalidof

ttc-example-mkuvalidqq: hr
ttc-example-mkuvalidqq: # build and run ttc-example-mkuvalidqq *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-mkuvalidqq
else
> cabal run $(CABAL_ARGS) ttc-example-mkuvalidqq
endif
.PHONY: ttc-example-mkuvalidqq

ttc-example-valid: hr
ttc-example-valid: # build and run ttc-example-valid *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-valid
else
> cabal run $(CABAL_ARGS) ttc-example-valid
endif
.PHONY: ttc-example-valid

ttc-example-validof: hr
ttc-example-validof: # build and run ttc-example-validof *
ifeq ($(MODE), stack)
> stack run $(STACK_ARGS) ttc-example-validof
else
> cabal run $(CABAL_ARGS) ttc-example-validof
endif
.PHONY: ttc-example-validof

ttc-repl: # ttc: enter a REPL *
ifeq ($(MODE), stack)
> stack repl ttc $(STACK_ARGS)
else
> cabal repl ttc $(CABAL_ARGS)
endif
.PHONY: ttc-repl

ttc-sdist: # ttc: create a source tarball for Hackage
> $(eval BRANCH := $(shell git rev-parse --abbrev-ref HEAD))
> @test "${BRANCH}" = "main" || $(call die,"not in main branch")
ifeq ($(MODE), stack)
> @stack sdist ttc
else
> @cabal sdist ttc
endif
.PHONY: ttc-sdist

ttc-test: hr
ttc-test: # ttc: run tests, optionally for pattern P *
> $(eval P := "")
ifeq ($(MODE), stack)
> @test -z "$(P)" \
>   && stack test ttc $(STACK_ARGS) \
>   || stack test ttc $(STACK_ARGS) --test-arguments '--pattern $(P)'
else
> @test -z "$(P)" \
>   && cabal test ttc $(CABAL_ARGS) \
>        --enable-tests --test-show-details=always \
>   || cabal test ttc $(CABAL_ARGS) \
>       --enable-tests --test-show-details=always \
>       --test-option '--pattern=$(P)'
endif
.PHONY: ttc-test

ttc-test-all: # ttc: run all configured tests and build examples using MODE
ifeq ($(MODE), stack)
> @make ttc-test-build CONFIG=stack-8.6.5.yaml
> @make ttc-test-build CONFIG=stack-8.8.4.yaml
> @make ttc-test-build CONFIG=stack-8.10.7.yaml
> @make ttc-test-build CONFIG=stack-9.0.2.yaml
> @make ttc-test-build CONFIG=stack-9.2.8.yaml
> @make ttc-test-build CONFIG=stack-9.4.8.yaml
> @make ttc-test-build CONFIG=stack-9.6.6.yaml
> @make ttc-test-build CONFIG=stack-9.8.2.yaml
> @make ttc-test-build CONFIG=stack-9.10.1.yaml
else
> @make ttc-test-build GHC_VERSION=8.6.5
> @make ttc-test-build GHC_VERSION=8.8.4
> @make ttc-test-build GHC_VERSION=8.10.7
> @make ttc-test-build GHC_VERSION=9.0.2
> @make ttc-test-build GHC_VERSION=9.2.8
> @make ttc-test-build GHC_VERSION=9.4.8
> @make ttc-test-build GHC_VERSION=9.6.6
> @make ttc-test-build GHC_VERSION=9.8.2
> @make ttc-test-build GHC_VERSION=9.10.1
endif
.PHONY: ttc-test-all

ttc-test-bounds-lower: # ttc: test lower bounds (Cabal only)
> @make ttc-test-build MODE=cabal \
>   CABAL_ARGS="--project-file=cabal-ttc-bounds-lower.project"
.PHONY: ttc-test-bounds-lower

ttc-test-bounds-upper: # ttc: test upper bounds (Cabal only)
> @make ttc-test-build MODE=cabal \
>   CABAL_ARGS="--project-file=cabal-ttc-bounds-upper.project"
.PHONY: ttc-test-bounds-upper

ttc-test-build: hr
ttc-test-build: ttc
ttc-test-build: ttc-test
ttc-test-build: ttc-doc-api
ttc-test-build: ttc-examples
ttc-test-build: # ttc: build, run tests, build API documentation, build examples *
.PHONY: ttc-test-build

ttc-test-nightly: # ttc: run tests for the latest Stackage nightly release (Stack only)
> @make ttc-test MODE=stack RESOLVER=nightly
.PHONY: ttc-test-nightly

version: # show current versions
> @echo "ttc           $(call get_version, ttc/ttc.cabal)"
> @echo "ttc-examples  $(call get_version, ttc-examples/ttc-examples.cabal)"
.PHONY: version
