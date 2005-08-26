## Classes for representing graphs

setClass("graph", representation(edgemode="character", "VIRTUAL"))

## Node Edge List representation
setClass("graphNEL",
         contains="graph",
         representation(nodes="vector", edgeL="list"),
         validity=function(object) validGraph(object))

## Incidence Matrix representation
setClass("graphIM", contains="graph",
         representation(inciMat="matrix",
                        nodes="environment"),
         validity=function(object) validGraphIM(object))


setClass("graphH",
         representation(graphID="Ruuid",
                        nodes="list",
                        label2nodeID="character",
                        edges="environment"),
         contains="graph")


setOldClass("dist")


setClass("distGraph",
         representation(Dist="dist"),
         prototype=list(edgemode="undirected"),
         contains="graph")


setClass("clusterGraph",
         representation(clusters="list"), contains="graph",
         prototype=list(edgemode="undirected"))


## Graph components
## Not currently being used...

setClass("propertyHolder", representation(property="list"),
         contains="VIRTUAL")


setClass("gNode",
         representation(label="character",
                        fromEdges="list",
                        toEdges="list",
                        edgeOrder="list",
                        nodeType="character",
                        nodeID="Ruuid"),
         contains="propertyHolder",
         prototype=list(nodeID=getuuid(), nodetype="unknown", label=""))


## why not have a constant guid for null graphs?
assign("nullgraphID", getuuid())

setClass("gEdge",
         representation(edgeID="Ruuid",
                        edgeType="character",
                        directed="logical",
                        bNode="Ruuid",    ##begin - if directed
                        eNode="Ruuid"),   ##end   - if directed
         contains="propertyHolder",
         prototype=list(edgeID=nullgraphID, edgeType="unknown",
                          directed=FALSE, bNode=nullgraphID,
                          eNode=nullgraphID))


setClass("simpleEdge",
         representation(edgeType="character",
                        weight="numeric",
                        directed="logical",
                        bNode="character",    ##begin - if directed
                        eNode="character"),   ##end   - if directed
         prototype=list(edgeType="unknown",
           directed=FALSE, bNode="", eNode="", weight=1))


setClass("propertySet", representation(data="environment"))


setClass("edgeSet", representation(attrList="list",
                                   keySep="character"),
         contains="propertySet")


## Misc classes

setClass("hashtable", representation(hashtable="environment"))


setClass("generalGraph", representation(nodes="hashtable",
                                        edges="hashtable"),
         contains="graph")


setClass("file")


setClass("connection")



