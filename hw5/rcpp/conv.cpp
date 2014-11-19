// Based on http://dirk.eddelbuettel.com/papers/rcpp_lmu_munich_2014.pdf

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector convolveCpp(NumericVector a, NumericVector b) 
{
    int na = a.size(), nb = b.size();
    int nab = na + nb - 1;
    
    NumericVector xab(nab);
    for (int i = 0; i < na; i++)
        for (int j = 0; j < nb; j++)
            xab[i + j] += a[i] * b[j];
    
    return xab;
}