basicProps <- list(weight=1, color="blue", friends=c("bob", "alice"))


testCreation <- function() {
    aset <- new("attrData", attrList=basicProps)
    checkEquals(TRUE, is(aset, "attrData"))
}


testDefaultAttributesGetting <- function() {
    aset <- new("attrData", attrList=basicProps)

    ## Get the entire list
    checkEquals(basicProps, attrDefaults(aset))

    ## Get a single attribute
    checkEquals(basicProps$weight, attrDefaults(aset, attr="weight"))
    checkEquals(basicProps$friends, attrDefaults(aset, attr="friends"))

    ## It is an error to ask for an undefined attr
    myCheckException(attrDefaults(aset, attr="NOSUCHATTRIBUTE"))
    ## You can only ask for one attr at a time
    myCheckException(attrDefaults(aset, attr=c("weight", "friends")))
}


testDefaultAttributesSetting <- function() {
    aset <- new("attrData", attrList=basicProps)

    ## Edit default value, type changes are allowed
    attrDefaults(aset, attr="weight") <- 100
    checkEquals(100, attrDefaults(aset, attr="weight"))

    ## Add a new attribute
    attrDefaults(aset, attr="size") <- c(1, 2)
    checkEquals(c(1, 2), attrDefaults(aset, attr="size"))
        
    ## This is sort of dangerous, but for now, we'll allow it.
    ## I would prefer the attributes to be write-once and there
    ## forever.  Or at least a special interface to remove unwanted...
    newProps <- list(dir="home", size=100)
    attrDefaults(aset) <- newProps
    checkEquals(newProps, attrDefaults(aset))
}


testItemGettingAndSetting <- function() {
    aset <- new("attrData", attrList=basicProps)

    checkEquals(1, attrDataItem(aset, x="k1", attr="weight"))
    checkEquals(rep(1, 3), attrDataItem(aset, x=letters[1:3], attr="weight"))
    attrDataItem(aset, x="k1", attr="weight") <- 900
    checkEquals(900, attrDataItem(aset, x="k1", attr="weight"))
}
    
    


## testNodeProps <- function() {
##     ## XXX:
##     ## You have to have all generics in NAMESPACE :-(
##     ## if you want to hide a utility method, you cannot do this to test it:
##     ## pkg:::foo(obj) <- 1 if it is a replace method.
##     nodePropList <- list(weight=1, color="blue", friends=c("bob", "alice"))
##     es <- new("nodeSet", attrList=nodePropList)

##     ## get defaults
##     checkEquals(nodePropList, basicProps(es, "n3")[[1]])
    
##     ## set custom properties for an node
##     someProps <- list(weight=400, color="red")
##     ##basicProps(es, "n1") <- someProps
##     basicProps(es, "n1") <- someProps
##     expect <- list(n1=c(someProps, nodePropList["friends"]))
##     checkEquals(expect, basicProps(es, "n1"))
##     checkEquals(expect[[1]], basicProps(es, "n1")[[1]])
##     ## can update
##     someProps <- list(weight=99, color="purple")
##     basicProps(es, "n1") <- someProps
##     expect <- list(n1=c(someProps, nodePropList["friends"]))
##     checkEquals(expect, basicProps(es, "n1"))

##     ## exception if properites contain unknown name
##     badNodeProps <- list(weight=400, published=TRUE, phone=900)
##     myCheckException(basicProps(es, "n2") <- badNodeProps)
## }


## testNodeSetCloning <- function() {
##     ## Verify that making a copy works as most R users will expect
##     nodePropList <- list(weight=1, color="blue", friends=c("bob", "alice"))
##     es <- new("nodeSet", attrList=nodePropList)

##     es2 <- es

##     basicProps(es, "n1") <- list(weight=888, color="red")

##     checkEquals(1, basicProps(es2, c("n1", "n2"))[[1]]$weight)
##     checkEquals(888, basicProps(es, c("n1", "n2"))[[1]]$weight)
## }


## ## Example of vectorized access to node properties
## ## testGetNodes <- function() {
## ##     es <- basicNodeSet()
## ##     got <- getNodes(es, from=c("n1", "n1"), to=c("n2", "n3"))
## ##     expect <- list("n1|n2"=basicProps,
## ##                    "n1|n3"=c(n1n3Props, basicProps["friends"])[names(basicProps)])
## ##     checkEquals(expect, got)
## ## }
## ##
## ## testRemoveNodes <- function() {
## ##     es <- basicNodeSet()
## ##     removeNodes(es, from=c("n1", "n1"), to=c("n2", "n3"))
## ##     checkEquals(character(0), ls(es@data))
## ## }

    
