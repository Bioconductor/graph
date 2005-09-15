#
# Test setup
#
simpleInciMat <- function() {
    ## Here's a simple graph for testing
    ##    a           b
    ##    |\         /|
    ##    | \___c___/ |
    ##    |     |     |
    ##    \     |     /
    ##     \____d____/
    ##
    ##
    mat <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0),
                  byrow=TRUE, ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    mat
}


simpleDirectedGraph <- function() {
    ## Here's a simple graph for testing
    ##    a           b
    ##    |\         /^
    ##    | \__>c<__/ |
    ##    |     ^     |
    ##    \     |     /
    ##     \___>d____/
    ##
    ##
    mat <- matrix(c(0, 0, 1, 1,
                    0, 0, 1, 0,
                    0, 0, 0, 0,
                    0, 1, 1, 0),
                  byrow=TRUE, ncol=4)
    rownames(mat) <- letters[1:4]
    colnames(mat) <- letters[1:4]
    mat
    new("graphIM", inciMat=mat, edgemode="directed")
}


testNodeDataDefaults <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    ## If no attributes have been defined, empty list.
    checkEquals(list(), nodeDataDefaults(g1))

    ## Can assign a named list
    myEdgeAttributes <- list(foo=1, bar="blue")
    nodeDataDefaults(g1) <- myEdgeAttributes
    checkEquals(myEdgeAttributes, nodeDataDefaults(g1))

    checkEquals(myEdgeAttributes$foo, nodeDataDefaults(g1, attr="foo"))

    nodeDataDefaults(g1, attr="size") <- 400
    checkEquals(400, nodeDataDefaults(g1, attr="size"))

    myCheckException(nodeDataDefaults(g1, attr="NOSUCHATTRIBUTE"))
    myCheckException(nodeDataDefaults(g1) <- list(1, 3, 4)) ## must have names
}


testEdgeDataDefaults <- function() {
    mat <- simpleInciMat()
    g1 <- new("graphIM", inciMat=mat)

    ## If no attributes have been defined, empty list.
    checkEquals(list(), edgeDataDefaults(g1))

    ## Can assign a named list
    myEdgeAttributes <- list(foo=1, bar="blue")
    edgeDataDefaults(g1) <- myEdgeAttributes
    checkEquals(myEdgeAttributes, edgeDataDefaults(g1))

    checkEquals(myEdgeAttributes$foo, edgeDataDefaults(g1, attr="foo"))

    edgeDataDefaults(g1, attr="size") <- 400
    checkEquals(400, edgeDataDefaults(g1, attr="size"))

    myCheckException(edgeDataDefaults(g1, attr="NOSUCHATTRIBUTE"))
    myCheckException(edgeDataDefaults(g1) <- list(1, 3, 4)) ## must have names
}


## testNodeDataGetting <- function() {
##     mat <- simpleInciMat()
##     g1 <- new("graphIM", inciMat=mat)
##     myEdgeAttributes <- list(size=1, dim=c(3, 3), name="fred")
##     nodeDataDefaults(g1) <- myEdgeAttributes

##     checkEquals("fred", nodeData(g1, "a", attr="name")[[1]])

##     someNodes <- c("a", "b")
##     expect <- as.list(c(1, 1))
##     names(expect) <- someNodes
##     checkEquals(expect, nodeData(g1, n=someNodes, attr="size"))

##     expect <- as.list(rep("fred", length(nodes(g1))))
##     names(expect) <- nodes(g1)
##     checkEquals(expect, nodeData(g1, attr="size"))
## }


testNodeDataSetting <- function() {
TRUE
}




## testNodeAttributes <- function() {
##     mat <- simpleInciMat()
##     g1 <- new("graphIM", inciMat=mat)
    
##     ## If nothing defined, empty list for now
##     checkEquals(list(), nodeAttributes(g1, n="a")[[1]])
    
##     ## Exception if node not found
##     myCheckException(nodeAttributes(g1, n="nosuchNode"))
##     myCheckException(nodeAttributes(g1, n=c("a", "b", "NOPE")))
                     
##     ## pickup default values
##     myNodeAttributes <- list(weight=1, color="blue")
##     nodeSetAttributes(g1) <- myNodeAttributes
##     checkEquals(myNodeAttributes, nodeAttributes(g1, n="d")[[1]])

##     ## Customize existing edges
##     nodeAttrs <- list(weight=800)
##     ## sets weight for nodes a and d
##     nodeAttributes(g1, c("a", "d")) <- nodeAttrs  
##     checkEquals(800, nodeAttributes(g1, "a")[[1]]$weight)
##     checkEquals(800, nodeAttributes(g1, "d")[[1]]$weight)
##     checkEquals("blue", nodeAttributes(g1, "a")[[1]]$color)

##     ## disallow assigning names not in nodeSetAttributes
##     badNodeAttributes <- list(weight=400, style="modern", type="fruit")
##     myCheckException(nodeAttributes(g1, "a") <- badNodeAttributes)
##     myCheckException(nodeAttributes(g1, c("a", "b")) <- badNodeAttributes)
## }


## #
## # Edge data
## #
## testEdgeSetAttributes <- function() {
##     mat <- simpleInciMat()
##     g1 <- new("graphIM", inciMat=mat)

##     ## If no attributes have been defined, empty list or NULL?
##     checkEquals(list(), edgeSetAttributes(g1))

##     myEdgeAttributes <- list(weight=1, color="blue")
##     ## TODO: make edgeSetAttributes a set-once property
##     ## ideally, this is a set-once property because otherwise
##     ## we'll run into consistency issues for edges that have customized
##     ## attributes
##     edgeSetAttributes(g1) <- myEdgeAttributes
##     checkEquals(myEdgeAttributes, edgeSetAttributes(g1))
##     ## So assigning into it again is an error
##     ##myCheckException(edgeSetAttributes(g1) <- myEdgeAttributes)
## }


## testEdgeSetAttr <- function() {
##     mat <- simpleInciMat()
##     g1 <- new("graphIM", inciMat=mat)

##     myCheckException(edgeSetAttr(g1, "noSuchAttr"))
    
##     ## You can add attributes via edgeSetAttr and redefine default values
##     edgeSetAttr(g1, "weight") <- 1
##     checkEquals(1, edgeSetAttr(g1, "weight"))
##     ## Note: the names is edgeSet Attr, not edge SetAttr, is this too confusing?

##     ## redefine
##     val <- list(a=1, b=2)
##     edgeSetAttr(g1, "weight") <- val
##     checkEquals(val, edgeSetAttr(g1, "weight"))

##     ## add
##     edgeSetAttr(g1, "color") <- "blue"

##     expect <- list(weight=val, color="blue")
##     checkEquals(expect, edgeSetAttributes(g1))
## }


## testEdgeAttributes <- function() {
##     mat <- simpleInciMat()
##     g1 <- new("graphIM", inciMat=mat)

##     ## If nothing defined, empty list for now
##     checkEquals(list(), edgeAttributes(g1, from="a", to="d")[[1]])

##     ## Exception if node not found
##     myCheckException(edgeAttributes(g1, from="a", to="nosuchnode"))
##     myCheckException(edgeAttributes(g1, from="nosuchnode", to="a"))

##     ## Exception if edge not found
##     myCheckException(edgeAttributes(g1, from="a", to="b"))


##     ## pickup default values
##     myEdgeAttributes <- list(weight=1, color="blue")
##     edgeSetAttributes(g1) <- myEdgeAttributes
##     checkEquals(myEdgeAttributes, edgeAttributes(g1, from="a", to="d")[[1]])

##     ## disallow assigning names not in edgeSetAttributes
##     badEdgeAttributes <- list(weight=400, style="modern", type="fruit")
##     myCheckException(edgeAttributes(g1, "a", "d") <- badEdgeAttributes)

##     ## Customize existing edges
##     edgeAttributes(g1, "a", "d") <- list(weight=800)
##     checkEquals(800, edgeAttributes(g1, "a", "d")[[1]]$weight)
##     checkEquals("blue", edgeAttributes(g1, "a", "d")[[1]]$color)
## }


## testEdgeAttributesVectorized <- function() {
##     g1 <- simpleDirectedGraph()
##     myEdgeAttributes <- list(weight=1, color="blue")
##     edgeSetAttributes(g1) <- myEdgeAttributes

##     eAttrs <- edgeAttributes(g1, from="a")
##     checkEquals(TRUE, setequal(c("a|c", "a|d"), names(eAttrs)))

##     ## test with to="missing"
##     myCheckException(edgeAttributes(g1, from="c"))
##     checkEquals("b|c", names(edgeAttributes(g1, from="b")))

##     ## test with from="missing"
##     myCheckException(edgeAttributes(g1, to="a"))
##     checkEquals("d|b", names(edgeAttributes(g1, to="b")))
##     expect <- paste(c("a", "b", "d"), "c", sep="|")
##     checkEquals(expect, names(edgeAttributes(g1, to="c")))

##     fr <- c("a", "a", "d", "b", "d")
##     to <- c("d", "c", "b", "c", "c")
##     eAttrs <- edgeAttributes(g1, from=fr, to=to)
##     expectNames <- paste(fr, to, sep="|")
##     checkEquals(expectNames, names(eAttrs))

    
## }


