# Based on http://dirk.eddelbuettel.com/papers/rcpp_lmu_munich_2014.pdf

library(Rcpp)
library(RcppArmadillo)
library(rbenchmark)

rmvnormR = function(n, mu, sigma)
{
    Y = matrix(rnorm(n*ncol(sigma)), nrow = n, ncol=ncol(sigma))

    return( matrix(rep(mu, n), nrow=n, byrow=TRUE) + Y %*% chol(sigma) )
}

sourceCpp("rmvnorm.cpp")


# Small Covariance Matrix

set.seed(123)
dim = 10
ncor = 40

n = 10000
mu = rep(0,dim)
sigma = matrix(0, ncol=dim, nrow=dim)

for(k in 1:ncor)
{
    i = sample(1:dim,1,replace=TRUE)
    j = sample(1:dim,1,replace=TRUE)
    cor = runif(1)/2

    sigma[i,j] = cor
    sigma[j,i] = cor
}

diag(sigma) = 1

benchmark(rmvnormR(n,mu,sigma), rmvnormCpp(n,mu,sigma), order="relative", replications=10)[,1:4]


# Large covariance matrix

library(clusterGeneration)
sigma=genPositiveDefMat(2000)$Sigma
mu = rep(0,2000)
n = 10000

benchmark(rmvnormR(n,mu,sigma), rmvnormCpp(n,mu,sigma), order="relative", replications=10)[,1:4]
