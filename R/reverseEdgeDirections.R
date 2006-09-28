reverseEdgeDirections <- function(g) {
    ## FIXME: This needs to fix edge attributes, but for now, we punt
    ## and we are only using node attrs here anyhow...
    gam <- as(g, "graphAM")
    nodeNames <- nodes(g)
    gam@adjMat <- t(gam@adjMat)
    colnames(gam@adjMat) <- nodeNames
    as(gam, "graphNEL")
}
