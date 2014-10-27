# Class website transition graph

duke = list("Google"        = list(edges   = c(2L, 3L, 4L),
                                   weights = c(0.3, 0.5, 0.2)),
            "Github"        = list(edges   = c(1L,2L,6L),
                                   weights = c(0.05, 0.1, 0.85)),
            "stat.duke.edu" = list(edges   = c(1L, 3L, 4L, 6L),
                                   weights = c(0.1, 0.3, 0.2, 0.4)),
            "cr173@stat"    = list(edges   = c(5L, 6L),
                                   weights = c(0.05, 0.95)),
            "Sta102"        = list(edges   = c(4L, 6L),
                                   weights = c(0.95, 0.05)),
            "Sta523"        = list(edges   = c(2L, 6L),
                                   weights = c(0.5, 0.5)))

# We can think about the webpage we are currently on as being modeled by a 
# discrete time Markov chain with the transition probabilities given by the 
# weights. Therefore, we want to covert our graph object into an adjacency 
# (transition) matrix.

get_adj_mat = function(g)
{
    n = length(g)

    m = matrix(0, ncol=n, nrow=n)

    colnames(m) = names(g)
    rownames(m) = names(g)

    for(i in 1:n)
        m[i, g[[i]]$edges] = g[[i]]$weights

    stopifnot(all(rowSums(m)==1))

    return(m)
}

duke_adj = get_adj_mat(duke)

# To find the stationary distribution we need some initial conditions:
# Google - 0.6
# Sta523 - 0.2
# Github - 0.2
# Everything else - 0

initial_state = matrix(c(0.6,0.2,0,0,0.2,0),nrow=1)
colnames(duke_adj) = names(duke)

# Iterated applications of the transition matrix gets us towards the stationary dist

initial_state %*% duke_adj
initial_state %*% duke_adj %*% duke_adj
initial_state %*% duke_adj %*% duke_adj %*% duke_adj
initial_state %*% duke_adj %*% duke_adj %*% duke_adj %*% duke_adj


# Find the stationary distribution by taking m^n until convergence

find_stationary_dist = function(m, eps=1e-6)
{
    old = m

    repeat {
        cur = old %*% old

        if (sum(abs(old-cur)) < eps)
            break

        old = cur
    }

    return(cur)
}

initial_state %*% find_stationary_dist(duke_adj)



# What happens if we modify or graph slightly so that there is now a sink?

i = length(duke)+1L

duke[["Youtube"]] = list(edges   = i,
                         weights = 1)

duke[["Google"]] = list(edges   = c(2L, 3L, 4L, i),
                        weights = c(0.25, 0.45, 0.15, 0.15))
#duke[["Github"]] = list(edges   = c(1L, 2L, 6L, i),
#                        weights = c(0.05, 0.1, 0.65, 0.2))
#duke[["Sta523"]] = list(edges   = c(2L, 6L, i),
#                        weights = c(0.4, 0.4, 0.2))


duke_adj = get_adj_mat(duke)
initial_state = matrix(c(0.6,0.2,0,0,0.2,0,0),nrow=1)

initial_state %*% find_stationary_dist(duke_adj)


# Another absobring Markov chain example

sink = list(list(edges   = c(1L),
                 weights = c(1)),
            list(edges   = c(1L, 3L),
                 weights = c(0.5, 0.5)),
            list(edges   = c(2L, 4L),
                 weights = c(0.5, 0.5)),
            list(edges   = c(3L, 5L),
                 weights = c(0.5, 0.5)),
            list(edges   = c(5L),
                 weights = c(1)))

matrix(0.2,nrow=1,ncol=5) %*% find_stationary_dist(get_adj_mat(sink))



# Clearly the straight Markov chain will not work, since any sinks will screw up the result.
# Page rank adds the idea of a random restart where the surfer will just decide to start over.

page_rank = function(T, init, d = 0.85, eps = 1e-6)
{
    s = init
    old = s

    repeat {
        s = d * (s %*% T) + (1-d) * init

        if (sum(abs(old-s)) < eps)
            break

        old = s
    }

    return(s)
}

# Duke website
page_rank(duke_adj, matrix(c(0.6,0.2,0,0,0.2,0,0),nrow=1), d=0.85)

# Sink
page_rank(get_adj_mat(sink), matrix(0.2,nrow=1,ncol=5), d=0.85)
