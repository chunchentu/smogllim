#define ARMA_DONT_PRINT_ERRORS
#include <RcppArmadillo.h>
#include <math.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// [[Rcpp::export]]
arma::mat mat_transpose(arma::mat m){
    return(m.t());
}

// [[Rcpp::export]]
arma::mat cpp_logsumexp(arma::mat x, int axis=1){
    // note that axis is 0-indexed
    arma::mat y = arma::max(x, axis);
    arma::mat x_;

    if(axis == 1){
        x_ = x.each_col() - y;
    } else {
        x_ = x.each_row() - y;
    }
    arma::mat s = y + arma::log(arma::sum(arma::exp(x_), axis));

    // check values
    arma::Col<arma::uword> nonfinite_idx = arma::find_nonfinite(y);
    s.elem(nonfinite_idx) = y.elem(nonfinite_idx);

    return(s);
}

// [[Rcpp::export]]
arma::mat cpp_loggausspdf(arma::mat X, arma::vec mu, arma::mat Sigma){
    // Input:
    // X: DxN
    // mu: Dx1
    // Sigma: DxD

    // Output:
    // y: 1xN

    int N = X.n_cols;
    int D = X.n_rows;

    arma::mat U;
    arma::mat X_ = X.each_col() - mu;
    arma::rowvec y(N);
    try{
        U = arma::chol(Sigma);
        arma::mat Q = arma::solve(U.t(), X_);
        arma::rowvec q = arma::sum(arma::pow(Q, 2), 0);
        double c = D*log(2*arma::datum::pi) + 2*arma::sum(arma::log(U.diag()));
        y = -(c+q)/2;
    } catch( const std::runtime_error exception){
        y.fill(-arma::datum::inf);
    }

    return(y);
}
