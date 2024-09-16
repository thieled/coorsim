// [[Rcpp::depends(RcppArmadillo)]]
#ifdef TBB
#include <tbb/tbb.h>
#ifdef ONETBB_SPEC_VERSION
using namespace oneapi; // only Windows R 4.3.x or later
#endif
#endif

#include <RcppArmadillo.h>
#include <unordered_map>

using namespace Rcpp;

// Global variable to store the number of threads
#ifdef TBB
int num_threads = tbb::this_task_arena::max_concurrency();
#else
int num_threads = 1;
#endif

//' Set number of threads
//'
//' @param threads Integer, setting the number of threads for parallel processing.
//'
// [[Rcpp::export]]
void set_num_threads(int threads) {
  if (threads > 0) {
    num_threads = threads;
  } else {
    Rcpp::stop("Number of threads must be positive.");
  }
}

//' Query Embeddings and Compute Cosine Similarities with TBB
//'
//' This function takes a numeric matrix containing embeddings and a list of post ID lists.
//' It performs two tasks sequentially to conserve memory:
//' 1. Queries the embeddings for each post ID list.
//' 2. Computes the cosine similarities between the first embedding and all other embeddings within each subset.
//'
//' The function returns a list of data frames where each data frame contains post ID pairs and their cosine similarities that exceed the given threshold.
//'
//' @param m A NumericMatrix containing the embedding data with row names representing post IDs.
//' @param post_id_lists A List of CharacterVector objects, where each CharacterVector represents a set of post IDs to query in the embedding matrix.
//' @param threshold A double value representing the threshold for cosine similarity. Only similarities above this threshold are included in the output.
//' @return A List of DataFrames. Each data frame contains three columns: "post_id" (the first post ID in the pair), "post_id_y" (the second post ID in the pair), and "similarity" (the cosine similarity between them).
//' @export
// [[Rcpp::export]]
List query_and_compute_similarities_tbb(NumericMatrix m, List post_id_lists, double threshold) {
   CharacterVector rownames_m = rownames(m);
   std::unordered_map<std::string, int> rowname_map;
   
   // Map row names to their indices for quick lookup
   for (int i = 0; i < rownames_m.size(); ++i) {
     rowname_map[as<std::string>(rownames_m[i])] = i;
   }
   
   std::vector<DataFrame> result_list(post_id_lists.size());
   
#ifdef TBB
   // Set the number of threads for TBB
   tbb::task_arena arena(num_threads);
   arena.execute([&]() {
     tbb::parallel_for(0, static_cast<int>(post_id_lists.size()), [&](int i) {
#else
       for (int i = 0; i < post_id_lists.size(); ++i) {
#endif
         CharacterVector post_ids = post_id_lists[i];
         NumericMatrix subset(post_ids.size(), m.ncol());
         CharacterVector subset_rownames(post_ids.size());
         
         // Query embeddings for the current list of post IDs
         for (int j = 0; j < post_ids.size(); ++j) {
           std::string post_id = as<std::string>(post_ids[j]);
           if (rowname_map.find(post_id) != rowname_map.end()) {
             int index = rowname_map[post_id];
             subset(j, _) = m(index, _);
             subset_rownames[j] = post_ids[j];
           }
         }
         
         subset.attr("dimnames") = List::create(subset_rownames, R_NilValue);
         
         // Calculate cosine similarities
         arma::rowvec first_row(subset.ncol());
         for (int k = 0; k < subset.ncol(); ++k) {
           first_row[k] = subset(0, k);
         }
         
         double norm_first_row = arma::norm(first_row, 2);
         
         std::vector<std::string> post_id;
         std::vector<std::string> post_id_y;
         std::vector<double> similarities;
         
         for (int j = 1; j < subset.nrow(); ++j) {
           arma::rowvec current_row(subset.ncol());
           for (int k = 0; k < subset.ncol(); ++k) {
             current_row[k] = subset(j, k);
           }
           
           double dot_product = arma::dot(first_row, current_row);
           double norm_current_row = arma::norm(current_row, 2);
           double cosine_similarity = dot_product / (norm_first_row * norm_current_row);
           
           if (cosine_similarity > threshold) {
             post_id.push_back(as<std::string>(subset_rownames[0]));
             post_id_y.push_back(as<std::string>(subset_rownames[j]));
             similarities.push_back(cosine_similarity);
           }
         }
         
         DataFrame result = DataFrame::create(
           Named("post_id") = post_id,
           Named("post_id_y") = post_id_y,
           Named("similarity") = similarities
         );
         
         result_list[i] = result;
         
#ifdef TBB
       }); // End of parallel_for
     }); // End of task_arena
#else
   } // End of regular for loop
#endif
   
   return wrap(result_list);
 }
