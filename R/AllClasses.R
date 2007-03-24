## Classes for representing graphs
setClass("attrData", representation(data="list",
                                    defaults="list"))

setClass("graph", representation(edgemode="character",
                                 edgeData="attrData",
                                 nodeData="attrData",
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


setOldClass("dist")


setClass("distGraph",
         representation(Dist="dist"),
         prototype=list(edgemode="undirected"),
         contains="graph")


setClass("clusterGraph",
         representation(clusters="list"), contains="graph",
         prototype=list(edgemode="undirected"))





## Misc classes

setClass("simpleEdge",
         representation(edgeType="character",
                        weight="numeric",
                        directed="logical",
                        bNode="character",    ##begin - if directed
                        eNode="character"),   ##end   - if directed
         prototype=list(edgeType="unknown",
           directed=FALSE, bNode="", eNode="", weight=1))
