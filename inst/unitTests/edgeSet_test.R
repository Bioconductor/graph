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


testEdgeProps <- function() {
    ## XXX:
    ## You have to have all generics in NAMESPACE :-(
    ## if you want to hide a utility method, you cannot do this to test it:
    ## pkg:::foo(obj) <- 1 if it is a replace method.
    edgePropList <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgePropList)

    ## get defaults
    checkEquals(edgePropList, edgeProps(es, from="n3", to="n4")[[1]])
    
    ## set custom properties for an edge
    someProps <- list(weight=400, color="red")
    edgeProps(es, from="n1", to="n2") <- someProps
    expect <- c(someProps, edgePropList["friends"])
    checkEquals(expect, edgeProps(es, "n1", "n2")[[1]])
    ## can update
    someProps <- list(weight=99, color="purple")
    edgeProps(es, from="n1", to="n2") <- someProps
    expect <- c(someProps, edgePropList["friends"])
    checkEquals(expect, edgeProps(es, "n1", "n2")[[1]])

    ## exception if properites contain unknown name
    badEdgeProps <- list(weight=400, published=TRUE, phone=900)
    myCheckException(edgeProps(es, "n1", "n2") <- badEdgeProps)
}


testEdgeSetCloning <- function() {
    ## Verify that making a copy works as most R users will expect
    edgePropList <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("edgeSet", attrList=edgePropList)

    es2 <- es

    edgeProps(es, "n1", "n2") <- list(weight=888, color="red")

    checkEquals(1, edgeProps(es2, "n1", "n2")[[1]]$weight)
    checkEquals(888, edgeProps(es, "n1", "n2")[[1]]$weight)
}


## Example of vectorized access to edge properties
## testGetEdges <- function() {
##     es <- basicEdgeSet()
##     got <- getEdges(es, from=c("n1", "n1"), to=c("n2", "n3"))
##     expect <- list("n1|n2"=edgeProps,
##                    "n1|n3"=c(n1n3Props, edgeProps["friends"])[names(edgeProps)])
##     checkEquals(expect, got)
## }
##
## testRemoveEdges <- function() {
##     es <- basicEdgeSet()
##     removeEdges(es, from=c("n1", "n1"), to=c("n2", "n3"))
##     checkEquals(character(0), ls(es@data))
## }

    
