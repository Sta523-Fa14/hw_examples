library(parallel)

## Naive 1

mat_mult1 = function(x,y)
{
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(ncol(x)==nrow(y))

    res = matrix(0, nrow=nrow(x),ncol=ncol(y))

    for(i in 1:nrow(x))
    {
        for(j in 1:ncol(y))
        {
            for(k in 1:nrow(x))
                res[i,j] = res[i,j] + x[i,k]* y[k,j]
        }
    }

    return(res)
}

## Vectorization

mat_mult2 = function(x,y)
{
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(ncol(x)==nrow(y))

    res = matrix(0, nrow=nrow(x),ncol=ncol(y))

    for(i in 1:nrow(x))
    {
        for(j in 1:ncol(y))
        {
            res[i,j] = sum(x[i,]*y[,j])
        }
    }

    return(res)
}

## Mat Vec Multiply

mat_mult3 = function(x,y)
{
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(ncol(x)==nrow(y))

        res = matrix(0, nrow=nrow(x),ncol=ncol(y))

    for(i in 1:nrow(x))
    {
        res[i,] = x[i,,drop=FALSE] %*% y
    }

    return(res)
}

## Mat Vec Multiply via lapply

mat_mult4 = function(x,y)
{
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(ncol(x)==nrow(y))

    do.call(rbind, lapply(1:nrow(x), function(i) x[i,,drop=FALSE] %*% y))
}


## Mat Vec Multiply via mclapply

mat_mult5 = function(x,y,mc.cores=8)
{
    stopifnot(is.matrix(x))
    stopifnot(is.matrix(y))
    stopifnot(ncol(x)==nrow(y))

    do.call(rbind, mclapply(1:nrow(x), function(i) x[i,,drop=FALSE] %*% y, mc.cores=mc.cores))
}

