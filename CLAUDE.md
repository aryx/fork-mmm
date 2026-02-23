# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a fork of MMM, a web browser written in OCaml originally by François Rouaix (last released 1997). The project modernizes the codebase while preserving its architecture. The browser uses Tk (via a vendored camltk in `external/ocamltk`) for its GUI.

## Build System

The project supports two build systems in parallel: **traditional Make** (primary, required for linking) and **dune** (used for compilation checks and library builds; full executable linking not yet wired up).

### Setup
```sh
./configure            # installs opam deps from mmm.opam
cd external/ocamltk && ./configure --with-config=./site.config && make && make opt && make install
```

### Building
```sh
make                   # bytecode build (make rec + make allbyte)
make opt               # native build
dune build             # dune-based build (checks all OCaml, does not link mmm executable)
```

### Individual targets
```sh
make -C src/www all    # build a single subdirectory
make depend            # regenerate dependencies
make clean
```

### Linting
```sh
make check             # runs osemgrep with semgrep.jsonnet rules
```

### Running
After `make`, the binaries are: `./mmm`, `./htparse`, `./mmm_remote`.

## Architecture

### Directory layout
- `libs/commons/` — shared utility library (`Cap`, `Common`, `Exit`, `FS`, `Fpath`, `Logs`, etc.)
- `libs/misc/` and `libs/i18n/` — miscellaneous and internationalization support
- `src/globals/` — version info
- `src/core/` — core abstractions: `Error`, `Feed`, `Condition`, `Low` (idle tasks), timer/fileevent backends
- `src/url/` — URL parsing
- `src/www/` — document model: `Document`, `Www`, `Hyper`, `Maps`, `Uri`
- `src/http/` — HTTP protocol: headers, auth, date parsing, MIME types
- `src/protocols/` — protocol handlers: `Cache`, `File`, `Mailto`
- `src/retrieve/` — retrieval engine: `Retrieve`, `Scheduler`, `Progress`
- `languages/html/` — HTML lexer/parser
- `gui/tk/` — Tk backend wiring (event loop, error display, auth dialogs, file input, etc.)
- `gui/viewers/` — content viewers: `Plain`, `Embed`, `Decoders`, `Save`
- `gui/display/` — HTML rendering: layout, fonts, forms, tables, image loading
- `gui/chrome/` — browser chrome: navigation (`Nav`), history, hotlist, preferences, main window (`Mmm`)
- `main/` — entry points: `main.ml` (browser), `htparse.ml` (standalone HTML checker), `main_remote.ml`
- `external/ocamltk/` — vendored camltk (do not modify)

### Key architectural patterns

**Capabilities system**: The codebase uses OCaml object types as extensible capability records (see `libs/commons/Cap.ml`). Dangerous I/O and system operations must be accessed via capability parameters (e.g., `Cap.network`, `Cap.open_in`, `Cap.exit`, `Cap.argv`). The `semgrep.jsonnet` rules enforce that raw `Unix.*` / `Sys.*` calls are not used directly outside of designated capability modules and excluded dirs.

**Backend indirection**: Core modules like `src/core/` expose `*_ref` mutable references for Tk-specific backends (timers, file events, idle tasks, error display). The Tk backends in `gui/tk/` fill these references at startup in `main.ml`.

**dune library names**: Each subdirectory builds a library with a `mmm.*` public name (e.g., `mmm.commons`, `mmm.www`, `mmm.html`, `mmm.retrieve`, `mmm.tk`, `mmm.gui`).

**Literate programming markers**: Source files use `(*s: ... *)` / `(*e: ... *)` / `(*x: ... *)` annotations — these are syncweb (literate programming tool) markers. Do not remove them. And when adding code, do not try to generate
them as this will confuse the syncweb tool. Just leave the existing comments
with markers as is, and also do not reindent them.

## Semgrep / Cap rules

`semgrep.jsonnet` bans direct use of `Unix.connect`, `Sys.getenv_opt`, `Sys.remove`, `open_in`, `Obj.magic`, `exit`, `Sys.argv` outside of capability modules. Excluded directories: `external/ocamltk`, `demos`, `extensions`. Some specific files are individually excluded (see the `paths.exclude` lists in `semgrep.jsonnet`).
