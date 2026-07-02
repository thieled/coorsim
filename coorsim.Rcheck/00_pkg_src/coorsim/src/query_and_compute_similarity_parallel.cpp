// [[Rcpp::depends(RcppArmadillo)]]
#ifdef TBB
#include <tbb/tbb.h>
#ifdef ONETBB_SPEC_VERSION
using namespace oneapi; // only Windows R 4.3.x or later
#endif
#endif

#include <RcppArmadillo.h>
#include <unordered_map>
#include <mutex>

using namespace Rcpp;

// Global variable to store the number of threads
#ifdef TBB
int num_threads = tbb::this_task_arena::max_concurrency();
#else
int num_threads = 1;
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
   
   // Thread-safe cache: pair -> cosine similarity (to avoid recomputation across groups)
   std::unordered_map<uint64_t, double> cache;
   cache.reserve(1024);
   std::mutex cache_mtx;
   
   std::vector<DataFrame> result_list(post_id_lists.size());
   
#ifdef TBB
   tbb::task_arena arena(num_threads);
   arena.execute([&]() {
     tbb::parallel_for(0, static_cast<int>(post_id_lists.size()), [&](int i) {
#else
       for (int i = 0; i < post_id_lists.size(); ++i) {
#endif
         CharacterVector post_ids = post_id_lists[i];
         
         // Collect VALID indices and corresponding IDs for this group, preserving order
         std::vector<uint32_t> idx; idx.reserve(post_ids.size());
         std::vector<std::string> ids; ids.reserve(post_ids.size());
         for (int j = 0; j < post_ids.size(); ++j) {
           std::string pid = Rcpp::as<std::string>(post_ids[j]);
           auto it = row_index.find(pid);
           if (it != row_index.end()) {
             idx.push_back(it->second);
             ids.push_back(std::move(pid));
           }
         }
         
         // If fewer than 2 valid IDs, result is empty (same shape as original)
         if (idx.size() <= 1) {
           result_list[i] = DataFrame::create(
             Named("post_id")    = CharacterVector(0),
             Named("post_id_y")  = CharacterVector(0),
             Named("similarity") = NumericVector(0)
           );
#ifdef TBB
           return;
#else
           continue;
#endif
         }
         
         // Anchor = first valid ID in this group (same as row 0 of original subset)
         uint32_t i0 = idx[0];
         const arma::rowvec row0 = M.row(i0);
         const double n0 = norms[i0];
         
         std::vector<std::string> out_a; out_a.reserve(idx.size() - 1);
         std::vector<std::string> out_b; out_b.reserve(idx.size() - 1);
         std::vector<double>      out_s; out_s.reserve(idx.size() - 1);
         
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
             out_a.push_back(ids[0]);   // group anchor ID
             out_b.push_back(ids[j]);   // current ID
             out_s.push_back(sim);
           }
         }
         
         result_list[i] = DataFrame::create(
           Named("post_id")    = out_a,
           Named("post_id_y")  = out_b,
           Named("similarity") = out_s
         );
         
#ifdef TBB
       }); // parallel_for
     });   // arena
#else
   }     // for
#endif
   
   return wrap(result_list);
}
