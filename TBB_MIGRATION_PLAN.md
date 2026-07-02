# Migration Plan: Custom TBB linkage → RcppParallel (Option 3)

## Goal
Make `coorsim`'s TBB-based parallel code portable and crash-free on all platforms
(Windows, macOS, Linux) by delegating TBB provisioning/linking to **RcppParallel**,
the CRAN-standard mechanism. Preserve current behavior; keep sequential fallback correct.

## Why
The custom `configure` / `Makevars.in` / `libtbb.R` approach linked TBB dynamically
(`-ltbb` on Linux, `-ltbb12` on Windows) and enabled the TBB code path whenever TBB
merely *compiled+linked*, never verifying it *runs*. On installs where the linked
`libtbb`/`tbb12.dll` was missing or ABI-mismatched at load time, the package
corrupted R's lazy-load DB → `stack imbalance in 'lazyLoadDBfetch'` → segfault.

RcppParallel bundles a known-good TBB and:
- Windows: `RcppParallel::LdFlags()` links the bundled TBB.
- Linux/macOS: TBB is loaded dynamically **by RcppParallel** at package load
  (requires `importFrom(RcppParallel, RcppParallelLibs)` in NAMESPACE).

## Changes

### 1. DESCRIPTION
- Add `RcppParallel` to `Imports` and `LinkingTo`.
- Remove `SystemRequirements: GNU make` (no longer need custom Makevars shell expansion)
  — but Windows Makevars will use `$(shell ...)`, which IS a GNU make extension, so
  KEEP `SystemRequirements: GNU make`.
- Keep `LinkingTo: Rcpp, RcppArmadillo (>= ...)`, add `RcppParallel`.

### 2. NAMESPACE (via roxygen)
- Add `#' @importFrom RcppParallel RcppParallelLibs` and
  `#' @importFrom Rcpp evalCpp` (already present via useDynLib) to a package-doc `.R`.
- Regenerate with roxygen so NAMESPACE gets `importFrom(RcppParallel,RcppParallelLibs)`.

### 3. Build system — REMOVE custom TBB detection
Delete / stop shipping:
- `configure`        (autoconf-generated)
- `configure.ac`
- `cleanup`
- `src/Makevars.in`
- `inst/libtbb.R`
Replace with a single, simple, portable pair:
- `src/Makevars`     (tracked again; no placeholders)
- `src/Makevars.win`

`src/Makevars`:
    PKG_CPPFLAGS = -DARMA_64BIT_WORD=1 -DTBB
    PKG_LIBS = $(shell "${R_HOME}/bin/Rscript" -e "RcppParallel::RcppParallelLibs()") \
               $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)

`src/Makevars.win`:
    PKG_CPPFLAGS = -DARMA_64BIT_WORD=1 -DTBB
    PKG_LIBS = $(shell "${R_HOME}/bin/Rscript" -e "RcppParallel::RcppParallelLibs()") \
               $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)

Note: keep `-DTBB` because RcppParallel guarantees TBB is available on all platforms,
so the `#ifdef TBB` code path is always safe to enable now.

### 4. C++ sources
- `src/query_and_compute_similarity_parallel.cpp`:
  - Add `#include <RcppParallel.h>` (before the tbb usage / after RcppArmadillo).
  - Keep existing `tbb::` calls (`task_arena`, `parallel_for`, `blocked_range`).
    RcppParallel pulls in the correct TBB headers via `-I` from LinkingTo.
  - Keep the lazy `resolve_num_threads()` (already added; avoids TBB call at dlopen).
- Verify the raw `tbb/*.h` includes still resolve; RcppParallel's include dir provides
  the bundled TBB headers, so keep `#include <tbb/...>` as-is or rely on
  `<RcppParallel.h>`. Prefer minimal edits: add `<RcppParallel.h>`, leave tbb includes.

### 5. .gitattributes / ignore files
- Remove now-obsolete `configure`/`cleanup`/`configure.ac` `eol=lf` entries
  (or leave harmless; but tidy: drop them since files are gone).
- `src/.gitignore`: stop ignoring `Makevars` (it is tracked again). Keep ignoring
  `*.o`, `*.so`, `*.dll`, `symbols.rds`.
- `.gitignore` / `.Rbuildignore`: remove `configure*` / autoconf artifact lines.

## Validation
1. `Rcpp::compileAttributes()` + `roxygen2::roxygenise()` (NAMESPACE, Rd).
2. Build tarball: `R CMD build .`.
3. **Staged install into temp lib** (the path that reproduced the crash):
   `R CMD INSTALL --library=<tmp> coorsim_*.tar.gz`.
4. Fresh `Rscript` smoke test loading from temp lib:
   run `detect_cosimilarity(... parallel=TRUE)` and `parallel=FALSE`; expect no segfault.
5. `R CMD check --as-cran` (or `devtools::check`): expect 0/0/0.

## Cleanup
- Delete generated artifacts: `coorsim.Rcheck/`, `coorsim_*.tar.gz`, temp lib, this test file.
- Remove `TBB_MIGRATION_PLAN.md`? Keep until user confirms, then optionally remove.

## Rollback
All changes are git-tracked on branch `tbb`; revert with `git checkout -- .` if needed.
