write_graph = function(g, file)
{
    stopifnot(!file.exists(file))
    stopifnot(file.exists(dirname(file)))

    write("",file=file)
}


read_graph = function(file)
{
    stopifnot(file.exists(file))

    return(list(list()))
}
