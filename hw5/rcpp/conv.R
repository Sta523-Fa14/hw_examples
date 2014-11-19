# Based on http://dirk.eddelbuettel.com/papers/rcpp_lmu_munich_2014.pdf

library(Rcpp)

convolveR = function(a, b) 
{
    xab = rep(0, length(a)+length(b)-1)
    for (i in 1:length(a))
        for (j in 1:length(b))
            xab[i + j] = xab[i + j] + a[i] * b[j]
    
    return(xab)
}

sourceCpp("conv.cpp")


x = rnorm(1000)
y = rnorm(1500)

benchmark(convolveR(x,y), convolveCpp(x,y), order="relative", replications=10)[,1:4]