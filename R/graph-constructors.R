# constructors for two subclasses of graph: graphNEL, graphAM
#

graphNEL <-
    function (nodes=character(), edgeL=list(), edgemode='undirected')
{
    new("graphNEL", nodes=nodes, edgeL=edgeL, edgemode=edgemode)
}


graphAM <-
    function (adjMat=matrix(integer(), 0, 0), edgemode='undirected',
              values=NA)
{
    if (length (values) == 1L && is.na (values))
        new("graphAM", adjMat=adjMat, edgemode=edgemode)
    else
        new("graphAM", adjMat=adjMat, edgemode=edgemode,
                 values=values)
}
