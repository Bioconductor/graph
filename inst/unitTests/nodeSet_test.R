nodeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
n1n3Props <- list(weight=2, color="red")


basicNodeSet <- function() {
    es <- new("nodeSet", attrList=nodeProps)
    checkEquals(character(0), ls(es@data))
    addNode2(es, from="n1", to="n2")
    addNode2(es, from="n1", to="n3", n1n3Props)
    es
}
    

testCreation <- function() {
    nodeProps <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("nodeSet", attrList=nodeProps)
    checkEquals(TRUE, is(es, "nodeSet"))
}


testNodeProps <- function() {
    ## XXX:
    ## You have to have all generics in NAMESPACE :-(
    ## if you want to hide a utility method, you cannot do this to test it:
    ## pkg:::foo(obj) <- 1 if it is a replace method.
    nodePropList <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("nodeSet", attrList=nodePropList)

    ## get defaults
    checkEquals(nodePropList, nodeProps(es, "n3")[[1]])
    
    ## set custom properties for an node
    someProps <- list(weight=400, color="red")
    ##nodeProps(es, "n1") <- someProps
    nodeProps(es, "n1") <- someProps
    expect <- list(n1=c(someProps, nodePropList["friends"]))
    checkEquals(expect, nodeProps(es, "n1"))
    checkEquals(expect[[1]], nodeProps(es, "n1")[[1]])
    ## can update
    someProps <- list(weight=99, color="purple")
    nodeProps(es, "n1") <- someProps
    expect <- list(n1=c(someProps, nodePropList["friends"]))
    checkEquals(expect, nodeProps(es, "n1"))

    ## exception if properites contain unknown name
    badNodeProps <- list(weight=400, published=TRUE, phone=900)
    myCheckException(nodeProps(es, "n2") <- badNodeProps)
}


testNodeSetCloning <- function() {
    ## Verify that making a copy works as most R users will expect
    nodePropList <- list(weight=1, color="blue", friends=c("bob", "alice"))
    es <- new("nodeSet", attrList=nodePropList)

    es2 <- es

    nodeProps(es, "n1") <- list(weight=888, color="red")

    checkEquals(1, nodeProps(es2, c("n1", "n2"))[[1]]$weight)
    checkEquals(888, nodeProps(es, c("n1", "n2"))[[1]]$weight)
}


## Example of vectorized access to node properties
## testGetNodes <- function() {
##     es <- basicNodeSet()
##     got <- getNodes(es, from=c("n1", "n1"), to=c("n2", "n3"))
##     expect <- list("n1|n2"=nodeProps,
##                    "n1|n3"=c(n1n3Props, nodeProps["friends"])[names(nodeProps)])
##     checkEquals(expect, got)
## }
##
## testRemoveNodes <- function() {
##     es <- basicNodeSet()
##     removeNodes(es, from=c("n1", "n1"), to=c("n2", "n3"))
##     checkEquals(character(0), ls(es@data))
## }

    
