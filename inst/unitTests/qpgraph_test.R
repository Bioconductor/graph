library(qtl)
library(graph)
library(qpgraph)

## simulate an eQTLnetwork, this code is taken from section 2 of the vignette
## entitled 'Estimate eQTL networks using qpgraph' from the 'qpgraph' package.
## this simulation uses the API of the 'graph' package, concretely calling
## 'subGraph()', 'nodes()', 'edges()', 'degree()', 'edgeData()', 'numEdges()',
## 'graphBAM()', 'nodeDataDefaults()', 'edgeDataDefaults()', 'nodeData()',
## 'edgeData()', 'numNodes()', 'numEdges()', 'edgemode()' and 'addEdge()'
test_eQTLnetwork <- function() {
    ## Simulate a genetic map using the R/CRAN package qtl, consisting of
    ## nine chromosomes, being 100 cM long with 10 markers equally spaced along
    ## each of them, no telomeric markers and no X sexual chromosome.
    map <- sim.map(len=rep(100, times=9),
                   n.mar=rep(10, times=9),
                   anchor.tel=FALSE,
                   eq.spacing=TRUE,
                   include.x=FALSE)

    ## Simulate an eQTL network consisting of 50 genes, where half of them have
    ## one cis-acting (local) eQTL, there are 5 eQTL trans-acting (distant) on
    ## 5 genes each, and each gene is connected to 2 other genes. Each eQTL has
    ## an additive effect of a=2 and each gene-gene association has a marginal
    ## correlation rho=0.5.
    set.seed(12345)
    sim.eqtl <- reQTLcross(eQTLcrossParam(map=map, genes=50, cis=0.5,
                                          trans=rep(5, 5),
                                          networkParam=dRegularGraphParam(d=2)),
                           a=2, rho=0.5)

    ## check that all simulated genes are connected to two other genes in the
    ## network
    checkTrue(all(degree(sim.eqtl$model$g, sim.eqtl$model$Y) == 3))
}
