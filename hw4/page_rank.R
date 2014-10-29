source("predicates.R")

# Construct adjacency matrix from graph

get_adj_mat = function(g, trans_mat=FALSE)
{
    stopifnot(is_valid(g))

    n = length(g)
    m = matrix(0, ncol=n, nrow=n)

    colnames(m) = names(g)
    rownames(m) = names(g)

    for(i in 1:n)
        m[i, g[[i]]$edges] = ifelse(trans_mat, g[[i]]$weights/sum(g[[i]]$weights), g[[i]]$weights)

    return(m)
}



# Find the stationary distribution by taking m^n until convergence

get_stationary_dist = function(m, eps=1e-6)
{
    stopifnot(all(rowSums(m)==1))

    old = m

    repeat {
        cur = old %*% old

        if (sum(abs(old-cur)) < eps)
            break

        old = cur
    }

    return(cur)
}



# Page rank adds the idea of a random restart to the Markov model

page_rank = function(t, init, d = 0.85, eps = 1e-6)
{
    stopifnot(all(rowSums(t) == 1))
    stopifnot(sum(init) == 1)
    stopifnot(ncol(init) == nrow(t))

    s = init
    old = s

    repeat {
        s = d * (s %*% t) + (1-d) * init

        if (sum(abs(old-s)) < eps)
            break

        old = s
    }

    return(s)
}
