simpleGraphNEL <- function() {
     set.seed(123)
     V <- letters[1:4]
     edL <- vector("list", length=4)
     names(edL) <- V
     edL[["a"]] <- list(edges=c(3, 4), weights=runif(2))
     edL[["b"]] <- list(edges=c(3, 4), weights=runif(2))
     edL[["c"]] <- list(edges=c(1, 2, 4), weights=runif(3))
     edL[["d"]] <- list(edges=c(1, 2, 3), weights=runif(3))
     gR <- new("graphNEL", nodes=V, edgeL=edL)
     gR
 }


testIsAdjacent <- function() {
    g1 <- simpleGraphNEL()

    checkEquals(FALSE, isAdjacent(g1, "a", "b"))
    checkEquals(TRUE, isAdjacent(g1, "a", "c"))

    expect <- c(FALSE, TRUE, TRUE)
    got <- isAdjacent(g1, c("a", "a", "a"), c("b", "c", "d"))
    checkEquals(expect, got)
}
