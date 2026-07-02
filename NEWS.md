# coorsim 0.0.8

## Major changes

* **Migrated TBB integration to RcppParallel** for portable, cross-platform parallel processing. This eliminates the custom `configure`/`Makevars.in` detection and resolves segfaults and build failures on Linux/macOS/Windows CI runners caused by ABI mismatches and missing system TBB libraries. (#PR)

* **Fixed thread-safety bug in parallel similarity computation.** The previous implementation called R/Rcpp API functions (`Rcpp::as`, `DataFrame::create`) from TBB worker threads, which corrupted R's single-threaded allocator and protection stack, causing segfaults and `lazyLoadDBfetch` errors on installed builds. The parallel code now:
  - Extracts all R inputs to C++ containers (`std::vector<std::string>`) on the main thread before parallelization.
  - Computes similarities in parallel using only thread-safe C++ types (`std`, Armadillo).
  - Assembles R `DataFrame` results on the main thread after the parallel region completes.

## Dependencies

* Added `RcppParallel` to `Imports` and `LinkingTo`.
* Removed custom TBB detection files: `configure`, `configure.ac`, `cleanup`, `src/Makevars.in`, `inst/libtbb.R`.

## Build system

* `src/Makevars` and `src/Makevars.win` now use `RcppParallel::RcppParallelLibs()` to link the bundled TBB.
* `NAMESPACE` imports `RcppParallelLibs` to ensure TBB is loaded on all platforms.
* Dropped `C++11` from `SystemRequirements` (modern R defaults to C++17).

# coorsim 0.0.7

* Initial CRAN-ready version with TBB parallel support (prior to RcppParallel migration).
