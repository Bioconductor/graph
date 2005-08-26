testCreation <- function() {
    edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgeProps)
    checkEquals(TRUE, is(es, "edgeSet"))
}


testAddAndGetEdge <- function() {
    edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgeProps)
    addEdge2(from="n1", to="n2", es)
    checkEquals(list(), getEdge(from="n1", to="n2", es))
    ## adding a duplicate raises an error
    myCheckException(addEdge2(from="n1", to="n2", es))
    ## verify nothing fishy going on w/ references
    es2 <- new("edgeSet")
    addEdge2(from="n1", to="n2", es2)

    ## TODO: should the returned property list have the default values
    ## filled in?
    eProps <- list(weight=2, color="red")
    addEdge2(from="n1", to="n3", object=es, eProps)
    checkEquals(eProps, getEdge(from="n1", to="n3", es))
}


testGetEdges <- function() {
    edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgeProps)
    checkEquals(character(0), ls(es@data))
    addEdge2(from="n1", to="n2", es)
    eProps <- list(weight=2, color="red")
    addEdge2(from="n1", to="n3", object=es, eProps)
    got <- getEdges(from=c("n1", "n1"), to=c("n2", "n3"), es)
    checkEquals(list("n1|n2"=list(), "n1|n3"=eProps), got)
}
    

testRemoveEdges <- function() {
    edgeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgeProps)
    addEdge2(from="n1", to="n2", es)
    eProps <- list(weight=2, color="red")
    addEdge2(from="n1", to="n3", object=es, eProps)
    removeEdges(from=c("n1", "n1"), to=c("n2", "n3"), es)
    checkEquals(character(0), ls(es@data))
}

    
