// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// timesTwo
NumericVector timesTwo(NumericVector x);
RcppExport SEXP _smogllim_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(timesTwo(x));
    return rcpp_result_gen;
END_RCPP
}
// mat_transpose
arma::mat mat_transpose(arma::mat m);
RcppExport SEXP _smogllim_mat_transpose(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_transpose(m));
    return rcpp_result_gen;
END_RCPP
}
// cpp_logsumexp
arma::mat cpp_logsumexp(arma::mat x, int axis);
RcppExport SEXP _smogllim_cpp_logsumexp(SEXP xSEXP, SEXP axisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type axis(axisSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_logsumexp(x, axis));
    return rcpp_result_gen;
END_RCPP
}
// cpp_loggausspdf
arma::mat cpp_loggausspdf(arma::mat X, arma::vec mu, arma::mat Sigma);
RcppExport SEXP _smogllim_cpp_loggausspdf(SEXP XSEXP, SEXP muSEXP, SEXP SigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Sigma(SigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_loggausspdf(X, mu, Sigma));
    return rcpp_result_gen;
END_RCPP
}
// test_List
List test_List(List cstr);
RcppExport SEXP _smogllim_test_List(SEXP cstrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type cstr(cstrSEXP);
    rcpp_result_gen = Rcpp::wrap(test_List(cstr));
    return rcpp_result_gen;
END_RCPP
}
// test_mat_assign
arma::mat test_mat_assign(arma::mat x);
RcppExport SEXP _smogllim_test_mat_assign(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(test_mat_assign(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_smogllim_timesTwo", (DL_FUNC) &_smogllim_timesTwo, 1},
    {"_smogllim_mat_transpose", (DL_FUNC) &_smogllim_mat_transpose, 1},
    {"_smogllim_cpp_logsumexp", (DL_FUNC) &_smogllim_cpp_logsumexp, 2},
    {"_smogllim_cpp_loggausspdf", (DL_FUNC) &_smogllim_cpp_loggausspdf, 3},
    {"_smogllim_test_List", (DL_FUNC) &_smogllim_test_List, 1},
    {"_smogllim_test_mat_assign", (DL_FUNC) &_smogllim_test_mat_assign, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_smogllim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
