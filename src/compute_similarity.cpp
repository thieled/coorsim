// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

//' Compute cosine similarities
 //'
 //' This function computes the cosine similarity between the first row and all other rows of each matrix in a list.
 //'
 //' @param matrices_list A list of matrices where the row names are document IDs and columns are embeddings.
 //' @param thr A threshold value for the cosine similarity.
 //' @return A list of data frames, each containing the document ID pairs and their cosine similarity values.
 //' @export
 // [[Rcpp::export]]
 List compute_cosine_similarities(List matrices_list, double thr) {
   std::vector<DataFrame> result_list;
   
   // Iterate over each matrix in the list
   for (int i = 0; i < matrices_list.size(); ++i) {
     // Get the current matrix and its rownames
     NumericMatrix mat = as<NumericMatrix>(matrices_list[i]);
     List dimnames = mat.attr("dimnames");
     CharacterVector rownames = as<CharacterVector>(dimnames[0]);
     
     // Get the first row vector
     arma::rowvec first_row(mat.ncol());
     for (int k = 0; k < mat.ncol(); ++k) {
       first_row[k] = mat(0, k);
     }
     
     double norm_first_row = arma::norm(first_row, 2);
     
     // Vectors to store the results
     std::vector<std::string> doc_id_x;
     std::vector<std::string> doc_id_y;
     std::vector<double> similarities;
     
     // Iterate over the remaining rows
     for (int j = 1; j < mat.nrow(); ++j) {
       // Get the current row vector
       arma::rowvec current_row(mat.ncol());
       for (int k = 0; k < mat.ncol(); ++k) {
         current_row[k] = mat(j, k);
       }
       
       // Compute the cosine similarity
       double dot_product = arma::dot(first_row, current_row);
       double norm_current_row = arma::norm(current_row, 2);
       double cosine_similarity = dot_product / (norm_first_row * norm_current_row);
       
       // Check if the similarity exceeds the threshold
       if (cosine_similarity > thr) {
         doc_id_x.push_back(as<std::string>(rownames[0]));
         doc_id_y.push_back(as<std::string>(rownames[j]));
         similarities.push_back(cosine_similarity);
       }
     }
     
     // Create a data frame for the result
     DataFrame result = DataFrame::create(
       Named("doc_id_x") = doc_id_x,
       Named("doc_id_y") = doc_id_y,
       Named("similarity") = similarities
     );
     
     result_list.push_back(result);
   }
   
   return wrap(result_list);
 }
 