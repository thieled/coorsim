// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <unordered_map>

using namespace Rcpp;

//' Query Embeddings and Compute Cosine Similarities
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
List query_and_compute_similarities(NumericMatrix m, List post_id_lists, double threshold) {
   CharacterVector rownames_m = rownames(m);
   std::unordered_map<std::string, int> rowname_map;
   
   // Map row names to their indices for quick lookup
   for (int i = 0; i < rownames_m.size(); ++i) {
     rowname_map[as<std::string>(rownames_m[i])] = i;
   }
   
   std::vector<DataFrame> result_list;
   
   // Iterate over each list of post IDs
   for (int i = 0; i < post_id_lists.size(); ++i) {
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
     
     rownames(subset) = subset_rownames;
     
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
     
     result_list.push_back(result);
     
     // Free memory by clearing the subset matrix
     subset = NumericMatrix(0);
   }
   
   return wrap(result_list);
 }
 