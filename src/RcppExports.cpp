// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// compute_cosine_similarities
List compute_cosine_similarities(List matrices_list, double threshold);
RcppExport SEXP _coorsim_compute_cosine_similarities(SEXP matrices_listSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type matrices_list(matrices_listSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_cosine_similarities(matrices_list, threshold));
    return rcpp_result_gen;
END_RCPP
}
// query_embedding
List query_embedding(NumericMatrix m, List post_id_lists);
RcppExport SEXP _coorsim_query_embedding(SEXP mSEXP, SEXP post_id_listsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< List >::type post_id_lists(post_id_listsSEXP);
    rcpp_result_gen = Rcpp::wrap(query_embedding(m, post_id_lists));
    return rcpp_result_gen;
END_RCPP
}
// query_and_compute_similarities
List query_and_compute_similarities(NumericMatrix m, List post_id_lists, double threshold);
RcppExport SEXP _coorsim_query_and_compute_similarities(SEXP mSEXP, SEXP post_id_listsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< List >::type post_id_lists(post_id_listsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(query_and_compute_similarities(m, post_id_lists, threshold));
    return rcpp_result_gen;
END_RCPP
}
// set_num_threads
void set_num_threads(int threads);
RcppExport SEXP _coorsim_set_num_threads(SEXP threadsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    set_num_threads(threads);
    return R_NilValue;
END_RCPP
}
// query_and_compute_similarities_tbb
List query_and_compute_similarities_tbb(NumericMatrix m, List post_id_lists, double threshold);
RcppExport SEXP _coorsim_query_and_compute_similarities_tbb(SEXP mSEXP, SEXP post_id_listsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< List >::type post_id_lists(post_id_listsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(query_and_compute_similarities_tbb(m, post_id_lists, threshold));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_coorsim_compute_cosine_similarities", (DL_FUNC) &_coorsim_compute_cosine_similarities, 2},
    {"_coorsim_query_embedding", (DL_FUNC) &_coorsim_query_embedding, 2},
    {"_coorsim_query_and_compute_similarities", (DL_FUNC) &_coorsim_query_and_compute_similarities, 3},
    {"_coorsim_set_num_threads", (DL_FUNC) &_coorsim_set_num_threads, 1},
    {"_coorsim_query_and_compute_similarities_tbb", (DL_FUNC) &_coorsim_query_and_compute_similarities_tbb, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_coorsim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
