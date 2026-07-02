// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]
#ifdef TBB
#include <RcppParallel.h>
#include <tbb/tbb.h>
#ifdef ONETBB_SPEC_VERSION
using namespace oneapi; // only Windows R 4.3.x or later
#endif
#endif

#include <RcppArmadillo.h>
#include <unordered_map>
#include <mutex>
#include <vector>
#include <string>
#include <cstdint>

using namespace Rcpp;

// Global variable to store the number of threads.
// 0 means "auto-detect on first use". We deliberately avoid calling into TBB
// here: initializing this at static/global scope would invoke TBB during
// dlopen (library load), which corrupts R's lazy-load database and causes
// segfaults / "stack imbalance in lazyLoadDBfetch" on installed builds.
int num_threads = 0;

#ifdef TBB
// Resolve the effective thread count lazily, computing the default only on
// first use rather than at library-load time.
static inline int resolve_num_threads() {
  if (num_threads <= 0) {
    num_threads = tbb::this_task_arena::max_concurrency();
  }
  return num_threads;
}
#endif

//' Set number of threads
//'
//' @name set_num_threads
//' @param threads Integer, setting the number of threads for parallel processing.
//' @export
// [[Rcpp::export]]
void set_num_threads(int threads) {
  if (threads > 0) {
    num_threads = threads;
  } else {
    Rcpp::stop("Number of threads must be positive.");
  }
}

// Compact, symmetric key for a pair of row indices
static inline uint64_t make_pair_key(uint32_t a, uint32_t b) {
  uint32_t x = a < b ? a : b;
  uint32_t y = a < b ? b : a;
  return (static_cast<uint64_t>(x) << 32) | static_cast<uint64_t>(y);
}

//' Query embeddings and compute cosine similarities (group-only, cached)
//'
//' @name query_and_compute_similarities_tbb
//' @param m NumericMatrix with embeddings; rownames are post IDs.
//' @param post_id_lists List of CharacterVector; each vector is one group.
//' @param threshold double; only similarities strictly greater than this are returned.
//' @return List of DataFrame (one per input group) with columns: post_id, post_id_y, similarity.
//' @export
// [[Rcpp::export]]
List query_and_compute_similarities_tbb(NumericMatrix m,
                                        List post_id_lists,
                                        double threshold) {
  // Map rownames -> row index (valid rows only)
  CharacterVector rn = rownames(m);
  std::unordered_map<std::string, uint32_t> row_index;
  row_index.reserve(rn.size());
  for (int i = 0; i < rn.size(); ++i) {
    row_index.emplace(Rcpp::as<std::string>(rn[i]), static_cast<uint32_t>(i));
  }

  // Armadillo zero-copy view and precomputed L2 norms
  arma::mat M(m.begin(), m.nrow(), m.ncol(), /*copy_aux_mem=*/false, /*strict=*/false);
  arma::vec norms = arma::sqrt(arma::sum(arma::square(M), 1));

  const std::size_t n_groups = static_cast<std::size_t>(post_id_lists.size());

  // IMPORTANT: The R/Rcpp API is NOT thread-safe. R's allocator and protection
  // stack must only be touched from the main thread. We therefore extract all
  // R data into plain C++ containers *before* the parallel region, compute in
  // parallel using only std/Armadillo types, and build the R DataFrames on the
  // main thread *after* the parallel region.
  std::vector<std::vector<std::string>> groups(n_groups);
  for (std::size_t i = 0; i < n_groups; ++i) {
    CharacterVector post_ids = post_id_lists[i];
    std::vector<std::string>& g = groups[i];
    g.reserve(post_ids.size());
    for (int j = 0; j < post_ids.size(); ++j) {
      g.push_back(Rcpp::as<std::string>(post_ids[j]));
    }
  }

  // Per-group result held in pure C++ (no SEXP) until after the parallel loop.
  struct GroupResult {
    std::vector<std::string> a;
    std::vector<std::string> b;
    std::vector<double>      s;
  };
  std::vector<GroupResult> results(n_groups);

  // Thread-safe cache: pair -> cosine similarity (avoids recomputation across groups)
  std::unordered_map<uint64_t, double> cache;
  cache.reserve(1024);
  std::mutex cache_mtx;

  auto process_group = [&](std::size_t i) {
    const std::vector<std::string>& post_ids = groups[i];

    // Collect VALID indices and corresponding IDs for this group, preserving order
    std::vector<uint32_t> idx; idx.reserve(post_ids.size());
    std::vector<std::string> ids; ids.reserve(post_ids.size());
    for (std::size_t j = 0; j < post_ids.size(); ++j) {
      auto it = row_index.find(post_ids[j]);
      if (it != row_index.end()) {
        idx.push_back(it->second);
        ids.push_back(post_ids[j]);
      }
    }

    // If fewer than 2 valid IDs, result stays empty (same shape as original)
    if (idx.size() <= 1) {
      return;
    }

    // Anchor = first valid ID in this group (same as row 0 of original subset)
    uint32_t i0 = idx[0];
    const arma::rowvec row0 = M.row(i0);
    const double n0 = norms[i0];

    GroupResult& gr = results[i];
    gr.a.reserve(idx.size() - 1);
    gr.b.reserve(idx.size() - 1);
    gr.s.reserve(idx.size() - 1);

    for (size_t j = 1; j < idx.size(); ++j) {
      uint32_t ij = idx[j];
      const double nj = norms[ij];
      if (n0 == 0.0 || nj == 0.0) continue;

      // Lookup or compute cosine for (i0, ij). We always EMIT for this group
      // if it passes threshold; cache is purely to skip recomputation.
      double sim;
      uint64_t key = make_pair_key(i0, ij);
      bool found = false;

      { // small critical section for lookup
        std::lock_guard<std::mutex> lock(cache_mtx);
        auto it = cache.find(key);
        if (it != cache.end()) { sim = it->second; found = true; }
      }

      if (!found) {
        sim = arma::dot(row0, M.row(ij)) / (n0 * nj);
        std::lock_guard<std::mutex> lock(cache_mtx);
        cache.emplace(key, sim);
      }

      if (sim > threshold) {
        gr.a.push_back(ids[0]);   // group anchor ID
        gr.b.push_back(ids[j]);   // current ID
        gr.s.push_back(sim);
      }
    }
  };

#ifdef TBB
  tbb::task_arena arena(resolve_num_threads());
  arena.execute([&]() {
    tbb::parallel_for(std::size_t(0), n_groups, [&](std::size_t i) {
      process_group(i);
    });
  });
#else
  for (std::size_t i = 0; i < n_groups; ++i) {
    process_group(i);
  }
#endif

  // Build R DataFrames on the main thread (single-threaded, R-API-safe).
  List result_list(n_groups);
  for (std::size_t i = 0; i < n_groups; ++i) {
    const GroupResult& gr = results[i];
    result_list[i] = DataFrame::create(
      Named("post_id")           = wrap(gr.a),
      Named("post_id_y")         = wrap(gr.b),
      Named("similarity")        = wrap(gr.s),
      Named("stringsAsFactors")  = false
    );
  }

  return result_list;
}