simpleGraphH <- function() {
    v <- lapply(letters[1:5], function(l) new("gNode", label=l))
    e <- vector("list", 2)
    e[[1]] <- new("gEdge", bNode=v[[1]], eNode=v[[2]])
    e[[2]] <- new("gEdge", bNode=v[[2]], eNode=v[[3]])
    g <- new("graphH", nodes=v, edges=e)
    g
}

testIsAdjacent <- function() {
    g1 <- simpleGraphH()

    checkEquals(TRUE, isAdjacent(g1, "a", "b"))
    checkEquals(c(TRUE, TRUE, FALSE), isAdjacent(g1, c("b", "b", "b"),
                                                 c("c", "a", "d")))
    checkEquals(FALSE, isAdjacent(g1, "a", "e"))
                
}

