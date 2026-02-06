.PHONY: all build test clean check-llvm ll bc run help

# If you're in a Rosetta/x86_64 terminal on Apple Silicon, forcing arm64 avoids
# OCaml/ld architecture mismatches. On non-Apple-Silicon systems, override with:
#   make ARCH=
ARCH ?= $(shell arch -arm64 /usr/bin/true >/dev/null 2>&1 && printf 'arch -arm64' || printf '')

DUNE ?= dune

LLVM_BIN ?= /opt/homebrew/opt/llvm/bin
LLVM_AS ?= $(LLVM_BIN)/llvm-as
LLI ?= $(LLVM_BIN)/lli

MINIC ?= _build/default/src/main.exe

FILE ?=
OUT_LL ?= out.ll
OUT_BC ?= out.bc

all: build test

build:
	$(ARCH) $(DUNE) build

check-llvm:
	@test -x "$(LLVM_AS)" || (echo "error: missing llvm-as at $(LLVM_AS) (set LLVM_BIN=/path/to/llvm/bin)" >&2; exit 1)
	@test -x "$(LLI)" || (echo "error: missing lli at $(LLI) (set LLVM_BIN=/path/to/llvm/bin)" >&2; exit 1)

test: build check-llvm
	$(ARCH) $(DUNE) runtest

clean:
	$(ARCH) $(DUNE) clean

ll: build
	@test -n "$(FILE)" || (echo "usage: make ll FILE=path/to/input.c [OUT_LL=out.ll]" >&2; exit 2)
	$(MINIC) "$(FILE)" -o "$(OUT_LL)"

bc: ll check-llvm
	$(LLVM_AS) "$(OUT_LL)" -o "$(OUT_BC)"

run: bc
	$(LLI) "$(OUT_BC)"

help:
	@printf '%s\n' \
	  'Targets:' \
	  '  make build                 Build the compiler' \
	  '  make test                  Run cram tests (needs llvm-as/lli)' \
	  '  make clean                 Clean build artifacts' \
	  '  make ll  FILE=a.c          Compile MiniC → LLVM IR (.ll)' \
	  '  make run FILE=a.c          Compile+assemble+execute via lli' \
	  '' \
	  'Variables:' \
	  '  ARCH=<cmd>                 Prefix for arm64 builds (default: auto)' \
	  '  LLVM_BIN=/path/to/llvm/bin Default: /opt/homebrew/opt/llvm/bin' \
	  '  OUT_LL=out.ll OUT_BC=out.bc'

