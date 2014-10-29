is_valid = function(g) 
{
    # incomplete
    all( c(class(g), sapply(g, class)) == "list" )
}


is_undirected = function(g)
{
    TRUE
}

is_isomorphic = function(g1, g2)
{
    TRUE
}

is_connected = function(g, v1, v2)
{
    TRUE
}
