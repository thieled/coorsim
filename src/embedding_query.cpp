#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;

//' Query Embedding Subset
//'
//' This function takes a NumericMatrix and a list of post ID lists, 
//' and returns a list of NumericMatrix subsets where each subset corresponds to 
//' the rows in the input matrix matched by the post IDs in each list.
//'
//' @param m A NumericMatrix containing the embedding data with row names.
//' @param post_id_lists A List of CharacterVector objects, where each CharacterVector
//' represents a set of post IDs.
//' @return A List of NumericMatrix objects. Each matrix in the list contains the
//' rows from the input matrix \code{m} that correspond to the post IDs in the
//' corresponding element of \code{post_id_lists}.
//' @export
//'
// [[Rcpp::export]]
List query_embedding(NumericMatrix m, List post_id_lists) {
  CharacterVector rownames_m = rownames(m);
  std::unordered_map<std::string, int> rowname_map;
  
  for (int i = 0; i < rownames_m.size(); ++i) {
    rowname_map[as<std::string>(rownames_m[i])] = i;
  }
  
  List result(post_id_lists.size());
  
  for (int i = 0; i < post_id_lists.size(); ++i) {
    CharacterVector post_ids = post_id_lists[i];
    NumericMatrix subset(post_ids.size(), m.ncol());
    CharacterVector subset_rownames(post_ids.size());
    
    for (int j = 0; j < post_ids.size(); ++j) {
      std::string post_id = as<std::string>(post_ids[j]);
      if (rowname_map.find(post_id) != rowname_map.end()) {
        int index = rowname_map[post_id];
        subset(j, _) = m(index, _);
        subset_rownames[j] = post_ids[j];
      }
    }
    
    rownames(subset) = subset_rownames;
    result[i] = subset;
  }
  
  return result;
}
