// Based on http://dirk.eddelbuettel.com/papers/rcpp_lmu_munich_2014.pdf

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat rmvnormCpp(int n, arma::vec mu, arma::mat sigma) 
{
    arma::mat Y = arma::randn<arma::mat>(n, sigma.n_cols);
    
    return( arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma) );
}
