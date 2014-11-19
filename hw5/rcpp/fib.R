# Based on http://dirk.eddelbuettel.com/papers/rcpp_lmu_munich_2014.pdf

library(benchmark)
library(Rcpp)

fibR = function(n)
{
    if (n < 2)
        return(n)
    return( fibR(n-1) + fibR(n-2) )
}

cppFunction("
    int fibCpp(int n) 
    {
        if (n < 2) 
            return(n);
        return( fibCpp(n-1) + fibCpp(n-2) );
    }
")

benchmark(fibR(25), fibCpp(25), order="relative", replications=10)[,1:4]