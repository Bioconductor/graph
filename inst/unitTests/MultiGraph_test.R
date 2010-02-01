set.seed(0x12a9b)


make_directed_MultiGraph <- function(use.factors = TRUE)
{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                      to=c("b", "c", "d", "a", "d"),
                      weight=c(1, 3.1, 5.4, 1, 2.2),
                      stringsAsFactors = use.factors)

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                      to=c("b", "c", "x", "y", "c", "a"),
                      weight=c(3.4, 2.6, 1, 1, 1, 7.9),
                      stringsAsFactors = use.factors)

    esets <- list(e1=ft1, e2=ft2)

    g <- MultiGraph(esets)
    list(esets=esets, g=g)
}

make_mixed_MultiGraph <- function(use.factors = TRUE)
{
    ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
                      to=c("b", "c", "d", "a", "d"),
                      weight=c(1, 3.1, 5.4, 1, 2.2),
                      stringsAsFactors = use.factors)

    ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
                      to=c("b", "c", "x", "y", "c", "a"),
                      weight=c(3.4, 2.6, 1, 1, 1, 7.9),
                      stringsAsFactors = use.factors)

    ft3 <- data.frame(from=c("a", "a", "x", "x", "x"),
                      to  =c("b", "c", "a", "y", "c"),
                      weight=c(1:5),
                      stringsAsFactors = use.factors)

    esets <- list(e1=ft1, e2=ft2, e3=ft3)

    g <- MultiGraph(esets, directed = c(TRUE, TRUE, FALSE))
    list(esets=esets, g=g)
}

randMultiGraph <- function(numNodes, numEdges)
{
    ftlist <- lapply(numEdges, function(ne) {
        graph:::randFromTo(numNodes, ne)
    })
    nn <- ftlist[[1L]]$nodes
    edgeSets <- lapply(ftlist, function(x) x[["ft"]])
    names(edgeSets) <- paste("e", seq_len(length(edgeSets)), sep="")
    MultiGraph(edgeSets, nodes = nn)
}

sort_esets <- function(esets)
{
    ## sorting is based on column major ordering
    lapply(esets, function(ft) {
        ft <- ft[order(ft$to, ft$from), ]
    })
}

make_unique_ft <- function(ftdata)
{
    ## ftdata is a list with components $nodes and $ft
    ## $ft is a data.frame with columns 'from', 'to', and 'weight'
    ft <- ftdata[["ft"]]
    el <- paste(ft[["from"]], ft[["to"]], sep = "\t")
    dups <- duplicated(el)
    ftdata[["ft"]] <- ft[!dups, ]
    ftdata
}

test_create_infer_nodes <- function()
{
    basic <- make_directed_MultiGraph()
    esets <- basic$esets
    g <- basic$g
    checkEquals(6L, numNodes(g))
    checkEquals(c(e1=5L, e2=6L), numEdges(g))
}

## test constructor with nodes arg given (singleton case)
test_bad_nodes_in_create <- function()
{
    basic <- make_directed_MultiGraph()
    esets <- basic$esets
    bad_values <- c(NA, "a\n", "", "z|a", "a\t")
    for (v in bad_values) {
        tmp <- esets
        tmp[[1]]$to[3] <- v
        checkException(MultiGraph(tmp))
    }
}

## TODO: test undirected and mixed directed/undirected edge sets for
## eweights w/ and w/o names

test_edgeWeights_create <- function()
{
    basic <- make_mixed_MultiGraph()
    esets <- basic$esets
    g <- basic$g

    checkEquals(6L, numNodes(g))
    checkEquals(c(e1=5L, e2=6L, e3=5L), numEdges(g))

    esets <- sort_esets(esets)
    got <- eweights(g)
    checkIdentical(list(e1 = esets[[1L]][, "weight"]), got[1])
    checkIdentical(list(e2 = esets[[2L]][, "weight"]), got[2])
    ## FIXME: this is failing.  undirected case requires more
    ## attention to sorting detail
# !!!   checkIdentical(list(e3 = esets[[3L]][, "weight"]), got[3])
}

test_edgeWeights_edge_names <- function()
{
    basic <- make_directed_MultiGraph()
    esets <- basic$esets
    g <- basic$g

    wv <- eweights(g, names.sep = "=>")
    esets <- sort_esets(esets)

    want <- paste(esets[[1]]$from, esets[[1]]$to, sep = "=>")
    checkEquals(want, names(wv[[1]]))
}

test_supports_self_loops <- function()
{
    esets <- list(e1 = data.frame(from = c("a", "a"), to = c("a", "b"), weight = c(1, 2)))
    g <- MultiGraph(esets)
    checkEquals(c(e1 = 2), numEdges(g))
}

## test_edgeMatrices <- function()
## {
##     ft1 <- data.frame(from=c("a", "a", "a", "b", "b"),
##                         to=c("b", "c", "d", "a", "d"),
##                       weight=c(1, 3.1, 5.4, 1, 2.2))

##     ft2 <- data.frame(from=c("a", "a", "a", "x", "x", "c"),
##                         to=c("b", "c", "x", "y", "c", "a"),
##                       weight=c(3.4, 2.6, 1, 1, 1, 7.9))

##     esets <- list(ft1, ft2)

##     g <- MultiGraph(esets)

##     ems <- edgeMatrices(g)
##     checkEquals(2L, length(ems))
##     checkEquals(c(2L, 5L), dim(ems[[1L]]))
##     checkEquals(c(2L, 6L), dim(ems[[2L]]))
##     ## we seem to be going the route of double
##     ## to allow support of large graphs
##     ## checkTrue(typeof(ems[[1L]]) == "integer")
##     ## checkTrue(typeof(ems[[2L]]) == "integer")

##     nn <- nodes(g)
##     esets <- sort_esets(esets)
##     ft1 <- esets[[1L]]
##     ft2 <- esets[[2L]]

##     gotft1 <- data.frame(from = nn[ems[[1L]][1L, ]],
##                          to = nn[ems[[1L]][2L, ]])
##     checkEquals(ft1$from, gotft1$from)
##     checkEquals(ft1$to, gotft1$to)

##     gotft2 <- data.frame(from = nn[ems[[2L]][1L, ]],
##                          to = nn[ems[[2L]][2L, ]])
##     checkEquals(ft2$from, gotft2$from)
##     checkEquals(ft2$to, gotft2$to)
## }

## test_extractGraph <- function()
## {
##     g <- make_directed_MultiGraph()$g
##     g1 <- extractGraph(g, which = 1L)
##     checkEquals("graphNEL", as.character(class(g1)))
##     checkEquals(nodes(g), nodes(g1))
##     checkEquals(5L, numEdges(g1))
## }

## test_extractGraph_large <- function()
## {
##     eCounts <- c(e1=5, e2=10, e3=25, e4=75)
##     eNames <- names(eCounts)
##     g <- randMultiGraph(10, eCounts)
##     for (i in seq_len(length(eCounts))) {
##         gnel <- extractGraph(g, i)
##         checkEquals(eCounts[[i]], numEdges(gnel))
##         checkEquals(nodes(g), nodes(gnel))
##         ## also verify extraction by name
##         checkEquals(eCounts[[i]], numEdges(extractGraph(g, eNames[i])))
##     }
## }

## test_edgeIntersect <- function()
## {
##     ftdata1 <- graph:::randFromTo(10, 5)
##     ftdata2 <- graph:::randFromTo(10, 50)
##     ftdata3 <- graph:::randFromTo(10, 50)

##     ftdata2$ft <- rbind(ftdata2$ft, ftdata1$ft)
##     ftdata3$ft <- rbind(ftdata3$ft, ftdata1$ft)

##     ftdata2 <- make_unique_ft(ftdata2)
##     ftdata3 <- make_unique_ft(ftdata3)

##     edgeSets <- lapply(list(ftdata1, ftdata2, ftdata3),
##                        function(x) x[["ft"]])
##     nn <- ftdata1[["nodes"]]
##     g <- MultiGraph(edgeSets, nn)

##     wSum <- function(x)
##     {
##         rowSums(do.call(cbind, x))
##     }
##     g2 <- edgeIntersect(g, weightFun = rowSums)
##     checkEquals(1L, length(numEdges(g2)))
##     checkEquals(5L, numEdges(g2)[[1L]])
##     checkTrue(all(eweights(g2)[[1L]] == 3L))
##     checkEquals(edgeMatrices(g)[[1L]], edgeMatrices(g2)[[1L]])
## }

## test_fromToList <- function()
## {
##     basic <- make_directed_MultiGraph(use.factors = FALSE)
##     got <- graph:::fromToList(basic[["g"]])
##     esets <- sort_esets(basic[["esets"]])
##     checkEquals(c("from", "to", "weight"), names(got[[1L]]))
##     checkEquals(c("from", "to", "weight"), names(got[[2L]]))
##     for (i in 1:2) {
##         for (cn in c("from", "to", "weight")) {
##             checkEquals(esets[[i]][[cn]], got[[i]][[cn]])
##         }
##     }
## }

## test_edgeUnion <- function()
## {
##     ftdata1 <- graph:::randFromTo(10, 5)
##     ftdata2 <- graph:::randFromTo(10, 50)
##     ftdata3 <- graph:::randFromTo(10, 50)

##     ftdata2$ft <- rbind(ftdata2$ft, ftdata1$ft)
##     ftdata3$ft <- rbind(ftdata3$ft, ftdata1$ft)

##     ftdata2 <- make_unique_ft(ftdata2)
##     ftdata3 <- make_unique_ft(ftdata3)

##     edgeSets <- lapply(list(ftdata1, ftdata2, ftdata3),
##                        function(x) x[["ft"]])
##     nn <- ftdata1[["nodes"]]
##     g <- MultiGraph(edgeSets, nn)

##     gi <- edgeIntersect(g)

##     ## drop edge attrs, default to edge weights 1
##     gu <- edgeUnion(g)

##     uEdgeSet <- make_unique_ft(list(nodes = nodes(gu),
##                                     ft = do.call(rbind, edgeSets)))[["ft"]]
##     uEdgeSet <- uEdgeSet[order(uEdgeSet[[2L]], uEdgeSet[[1L]]), ]
##     row.names(uEdgeSet) <- NULL
##     got <- graph:::fromToList(gu)[[1L]]
##     checkEquals(uEdgeSet, got)

##     gu2 <- edgeUnion(g, function(x) rowSums(x, na.rm = TRUE))
##     ew <- eweights(gu2)[[1L]]
##     ## sanity check counts for now, would be good to have
##     ## some more complete tests
##     checkEquals(numEdges(gi)[[1L]], sum(ew == 3L))
##     checkTrue(!any(ew > 3))
##     checkTrue(!any(is.na(ew)))
## }

## test_large_node_graph <- function()
## {
##     ## here we test the case of a graph with a large node set, yet very
##     ## sparse edge sets.  The issue is that it is easy to overflow R's
##     ## integer vectors when vector indices have a max size of n^2.
##     esets <- list(data.frame(from="0000049998", to="0000049999", weight=1L))
##     nodeNames <- sprintf("%010d", seq_len(50000L))
##     g <- MultiGraph(esets, nodeNames)
##     checkEquals(1L, numEdges(g)[[1L]])

##     big <- graph:::randFromTo(50000, 500)
##     g <- MultiGraph(list(big$ft), big$nodes)
##     checkEquals(500L, numEdges(g)[[1L]])
## }
