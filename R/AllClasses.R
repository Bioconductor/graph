## Classes for representing graphs
setClass("attrData", representation(data="list",
                                    defaults="list"))

## (FH Oct 4.) The edgemode slot is deprecated, the information will
## go into the egemode item of the graphData list.



## class to hold information regarding rendering of a graph.  Doesn't
## make sense except as a slot in a graph (the names have to match)

setClass("renderInfo", 
         representation(nodes="list", # information on nodes
                        edges="list", # information on edges
                        graph="list",
                        pars="list")) # list passed on to graph.par before rendering


setClass("graph", representation(## edgemode="character",
                                 edgeData="attrData",
                                 nodeData="attrData",

                                 renderInfo="renderInfo",
                                 ## nodeInfo="list",
                                 ## edgeInfo="list",

                                 graphData="list",
                                 "VIRTUAL"))

## Node Edge List representation
setClass("graphNEL",
         contains="graph",
         representation(nodes="vector", edgeL="list"),
         validity=function(object) validGraph(object))


## Adjacency Matrix representation

setClass("graphAM", contains="graph",
         representation(adjMat="matrix"),
         validity=function(object) validGraph(object))

setClass("graphAM2", contains="graph",
         representation(bitVect="raw",
                        nodes="character"))

setOldClass("dist")


setClass("distGraph",
         representation(Dist="dist"),
         prototype=list(graphData=list(edgemode="undirected")),
         contains="graph")

setClass("clusterGraph",
         representation(clusters="list"), contains="graph",
         prototype=list(graphData=list(edgemode="undirected")))





## Misc classes

setClass("simpleEdge",
         representation(edgeType="character",
                        weight="numeric",
                        directed="logical",
                        bNode="character",    ##begin - if directed
                        eNode="character"),   ##end   - if directed
         prototype=list(edgeType="unknown",
           directed=FALSE, bNode="", eNode="", weight=1))



##multigraphs - not clear if we should extend graph here, or have a
##whole new set of classes

##looks like redefining edgeSets is going to help us out here.

setClass("edgeSet", contains="VIRTUAL",
         representation=representation(
           edgeData="attrData"))

setClass("edgeSetNEL", contains="edgeSet",
         representation(edgemode="character", edgeL="list"))

setClass("edgeSetAM", contains="edgeSet",
         representation(edgemode="character", adjMat="matrix"))

setClass("multiGraph",
         representation(nodes="vector", edgeL="list",
                        nodeData="attrData",
                        graphData="list"))


setClass("MultiDiGraph",
         representation = representation(nodes = "character",
         edgeAttrs = "list"),
         prototype = prototype(nodes = character(0)))
