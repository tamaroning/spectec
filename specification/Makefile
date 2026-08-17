EXE = $(PWD)/../spectec/spectec


# Main targets

.PHONY: all ci

all: check-all spec test
ci: check-all spec-clean spec test-clean test


# Checking

check: check-latest

check-1.0: $(EXE)
	$(EXE) wasm-1.0/*
	@echo Wasm 1.0 spec checked OK.
	@echo =========================

check-2.0: $(EXE)
	$(EXE) wasm-2.0/*
	@echo Wasm 2.0 spec checked OK.
	@echo =========================

check-3.0: $(EXE)
	$(EXE) wasm-3.0/*
	@echo Wasm 3.0 spec checked OK.
	@echo =========================

check-latest: $(EXE)
	$(EXE) wasm-latest/*
	@echo Wasm latest spec checked OK.
	@echo ============================

check-all: check-1.0 check-2.0 check-3.0 check-latest
	@echo All Wasm specs checked OK.

check-diff:
	bash ./diff-wasm-latest.sh


# Sync

sync-diff:
	bash ./sync-wasm-latest.sh


# Build

spec:
	(cd ../document/core && make main)
	@echo Wasm latest specs all built OK.
	@echo ===============================

spec-pdf:
	(cd ../document/core && make pdf)
	@echo Wasm latest PDF spec built OK.
	@echo ==============================

spec-html:
	(cd ../document/core && make html)
	@echo Wasm latest HTML spec built OK.
	@echo ===============================

spec-clean:
	(cd ../document/core && make clean)


# Full test

test:
	@((cd ../spectec/src && make test) || @echo Backend tests failed. Perhaps run `make testpromote` to update test output.)
	@echo Wasm latest backend tests ran OK.
	@echo =================================

test-clean:
	@(cd ../spectec/src && make clean)

testpromote:
	@(cd ../spectec/src && make testpromote)


# Lacuna export

export-lacuna: export-lacuna-wasm-1.0 export-lacuna-wasm-2.0 export-lacuna-wasm-3.0
	@echo All Wasm lacuna exports written OK.

export-lacuna-wasm-1.0: $(EXE)
	$(EXE) wasm-1.0/*.spectec --export-lacuna -o ../export/wasm-1.0
	@echo Wasm 1.0 lacuna export written to ../export/wasm-1.0

export-lacuna-wasm-2.0: $(EXE)
	$(EXE) wasm-2.0/*.spectec --export-lacuna -o ../export/wasm-2.0
	@echo Wasm 2.0 lacuna export written to ../export/wasm-2.0

export-lacuna-wasm-3.0: $(EXE)
	$(EXE) wasm-3.0/*.spectec --export-lacuna -o ../export/wasm-3.0
	@echo Wasm 3.0 lacuna export written to ../export/wasm-3.0


# Executable

$(EXE): spectec

spectec:
	@(cd ../spectec && make exe)
