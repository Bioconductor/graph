edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
n1n3Props <- list(weight=2, color="red")


basicEdgeSet <- function() {
    es <- new("edgeSet", attrList=edgeProps)
    checkEquals(character(0), ls(es@data))
    addEdge2(es, from="n1", to="n2")
    addEdge2(es, from="n1", to="n3", n1n3Props)
    es
}
    

testCreation <- function() {
    edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgeProps)
    checkEquals(TRUE, is(es, "edgeSet"))
}


testAddAndGetEdge <- function() {
    edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgeProps)
    addEdge2(es, from="n1", to="n2")
    ## you get the default attributes if you don't specify any when adding
    checkEquals(edgeProps, getEdge(es, from="n1", to="n2"))
    
    ## adding a duplicate raises an error
    myCheckException(addEdge2(es, from="n1", to="n2"))
    
    ## verify nothing fishy going on w/ references
    es2 <- new("edgeSet")
    addEdge2(es2, from="n1", to="n2")

    eProps <- list(weight=2, color="red")
    expectProps <- c(eProps, edgeProps["friends"])
    addEdge2(es, from="n1", to="n3", eProps)
    checkEquals(expectProps, getEdge(es, from="n1", to="n3")[names(expectProps)])
}


testGetEdges <- function() {
    es <- basicEdgeSet()
    got <- getEdges(es, from=c("n1", "n1"), to=c("n2", "n3"))
    expect <- list("n1|n2"=edgeProps,
                   "n1|n3"=c(n1n3Props, edgeProps["friends"])[names(edgeProps)])
    checkEquals(expect, got)
}
    

testRemoveEdges <- function() {
    es <- basicEdgeSet()
    removeEdges(es, from=c("n1", "n1"), to=c("n2", "n3"))
    checkEquals(character(0), ls(es@data))
}

    
## testEdgeAttr <- function() {
##     es <- basicEdgeSet()
##     propList <- edgeAttr(es, from="n1", to="n2", attrNames="color")

## }
