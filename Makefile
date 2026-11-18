# Makefile for CLIGEN v5 (fixed-form FORTRAN 77)
#
# Quick start:
#   make                 # build bin/cligen
#   make help            # list targets and variables
#   make run ARGS="-i<station.par> -o<out.cli> -t5 -b1 -y30"
#
# CLIGEN options take no space between the flag and its value (e.g. -y30).

# GNU make predefines FC=f77, so `?=` would not apply; override only that default.
ifeq ($(origin FC),default)
FC      := gfortran
endif
# -std=legacy     : accept old F77 constructs without errors
# -fno-automatic  : keep local variables between calls (SAVE semantics), which
#                   old F77 code often depends on. Several routines already use
#                   SAVE explicitly; this covers any that rely on it implicitly.
FFLAGS  ?= -O2 -std=legacy -fno-automatic
DBGFLAGS = -O0 -g -std=legacy -fno-automatic -fcheck=all -fbacktrace \
           -ffpe-trap=invalid,zero,overflow -Wall -Wno-tabs
LDFLAGS ?=

SRC     := cligen.f
INCS    := $(wildcard *.inc)
BINDIR  := bin
BIN     := $(BINDIR)/cligen
DBGBIN  := $(BINDIR)/cligen-debug

# Arguments passed to cligen by `make run`
ARGS    ?=

.PHONY: all build debug run run-debug version usage clean help check-fc

all: build

build: $(BIN)

# Compile from the repo root so the `include '*.inc'` statements resolve.
$(BIN): $(SRC) $(INCS) | check-fc $(BINDIR)
	$(FC) $(FFLAGS) -o $@ $(SRC) $(LDFLAGS)

debug: $(DBGBIN)

$(DBGBIN): $(SRC) $(INCS) | check-fc $(BINDIR)
	$(FC) $(DBGFLAGS) -o $@ $(SRC) $(LDFLAGS)

$(BINDIR):
	mkdir -p $@

check-fc:
	@command -v $(FC) >/dev/null 2>&1 || { \
	  echo "error: Fortran compiler '$(FC)' not found."; \
	  echo "  Ubuntu/Debian: sudo apt install gfortran"; \
	  echo "  Homebrew:      brew install gcc"; \
	  echo "  Or choose another compiler: make FC=ifx"; \
	  exit 1; }

# Run CLIGEN. With no ARGS it starts in interactive mode.
run: $(BIN)
	./$(BIN) $(ARGS)

run-debug: $(DBGBIN)
	./$(DBGBIN) $(ARGS)

# CLIGEN's -v prints the version and then continues into interactive mode,
# so feed it no input and keep only the version line.
version: $(BIN)
	@./$(BIN) -v </dev/null 2>/dev/null | grep -m1 'CLIGEN'

usage: $(BIN)
	./$(BIN) -h

clean:
	rm -rf $(BINDIR) *.o *.mod

help:
	@echo "Targets:"
	@echo "  build      Build $(BIN) (default)"
	@echo "  debug      Build $(DBGBIN) with runtime checks and backtraces"
	@echo "  run        Build and run: make run ARGS=\"-i<par> -o<out.cli> -t5 -b1 -y30\""
	@echo "  run-debug  Same as run, using the debug build"
	@echo "  version    Print the CLIGEN version (-v)"
	@echo "  usage      Print CLIGEN's command-line help (-h)"
	@echo "  clean      Remove build outputs"
	@echo ""
	@echo "Variables (override on the command line):"
	@echo "  FC=$(FC)  FFLAGS=\"$(FFLAGS)\"  ARGS=\"$(ARGS)\""
