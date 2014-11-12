library(parallel)
library(stringr)
library(magrittr)

mapreduce = function(map_func, reduce_func, mc.cores = detectCores()/2)
{
    stopifnot(is.function(map_func))
    stopifnot(is.function(reduce_func))

    function(x)
    {
        # Mapper
        map_res = mclapply(x, map_func, mc.cores = mc.cores)
        map_res = unlist(map_res, recursive=FALSE)

        # Partitioner
        nm = names(map_res) %>% unique() %>% sort()
        part_res = lapply(nm, function(x) unlist(map_res[names(map_res)==x])) %>% 
                   setNames(nm)

        # Reducer
        reduce_res = mclapply(part_res, reduce_func, mc.cores = mc.cores)

        return(reduce_res)
    }
}

f = mapreduce(function(x) list(sum=x), sum)
f(1)
f(1:10)
f(1:1000)



get_words = function(file)
{
    r = readLines(file) %>%
        str_replace_all("[[:punct:]]","") %>%
        tolower() %>%
        str_split(" ") %>%
        unlist() %>%
        str_trim()

    return( r[r != ""] %>% table() %>% as.list() )
}


f = mapreduce(get_words, sum)

macbeth = f("Shakespeare/macbeth.txt")
hamlet = f("Shakespeare/hamlet.txt")

macbeth[order(unlist(macbeth))]

complete = f(dir("Shakespeare",full.names=TRUE))
