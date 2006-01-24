## if (!isGeneric("foo"))
##   setGeneric("foo", function(x, ...) standardGeneric("foo"))

## Generic methods that all graph representation classes must support

setGeneric("isDirected", function(object) standardGeneric("isDirected"))

## ---------------------------------------------------------------------
## Node and edge access
## ---------------------------------------------------------------------
setGeneric("getNodes", function(x, which) standardGeneric("getNodes"))


setGeneric("edgemode", function(object) standardGeneric("edgemode"))


setGeneric("edgemode<-", function(object, value) standardGeneric("edgemode<-"))


setGeneric("nodes", function(object, ...) standardGeneric("nodes"))


setGeneric("nodes<-", function(object, value) standardGeneric("nodes<-"))


setGeneric("edges", function(object, which) standardGeneric("edges"))


setGeneric("edgeWeights", function(object, index)
           standardGeneric("edgeWeights"))
## ---------------------------------------------------------------------


## ---------------------------------------------------------------------
## Node and edge operations
## ---------------------------------------------------------------------
setGeneric("degree", function(object, Nodes) standardGeneric("degree"))


setGeneric("adj", function(object, index) standardGeneric("adj"))


setGeneric("acc", function(object, index) standardGeneric("acc"))


setGeneric("numNodes", function(object) standardGeneric("numNodes"))


setGeneric("numEdges", function(object) standardGeneric("numEdges"))


## default function numNoEdges(objGraph) already exists
##setGeneric("numNoEdges", function(object) standardGeneric("numNoEdges"))


setGeneric("addNode", function(node, object, edges) standardGeneric("addNode"))


setGeneric("removeNode", function(node, object) standardGeneric("removeNode"))


setGeneric("clearNode", function(node, object) standardGeneric("clearNode"))


setGeneric("combineNodes", function(nodes, graph, newName)
           standardGeneric("combineNodes"))


setGeneric("addEdge", function(from, to, graph, weights)
           standardGeneric("addEdge"))


setGeneric("removeEdge", function(from, to, graph)
           standardGeneric("removeEdge"))


setGeneric("removeEdges", function(container, from, to, ...)
           standardGeneric("removeEdges"))
## ---------------------------------------------------------------------


## ---------------------------------------------------------------------
## attrData generics
## ---------------------------------------------------------------------
setGeneric("attrDefaults", function(self, attr) standardGeneric("attrDefaults"))


setGeneric("attrDefaults<-", function(self, attr, value)
           standardGeneric("attrDefaults<-"))


setGeneric("attrDataItem", function(self, x, attr)
           standardGeneric("attrDataItem"))


setGeneric("attrDataItem<-", function(self, x, attr, value)
           standardGeneric("attrDataItem<-"))


## ---------------------------------------------------------------------
## node and edge attribute generics
## ---------------------------------------------------------------------
setGeneric("edgeDataDefaults", function(self, attr)
           standardGeneric("edgeDataDefaults"))


setGeneric("edgeDataDefaults<-", function(self, attr, value)
           standardGeneric("edgeDataDefaults<-"))


setGeneric("nodeDataDefaults", function(self, attr)
           standardGeneric("nodeDataDefaults"))


setGeneric("nodeDataDefaults<-", function(self, attr, value)
           standardGeneric("nodeDataDefaults<-"))


setGeneric("nodeData", function(self, n, attr)
           standardGeneric("nodeData"))


setGeneric("nodeData<-", function(self, n, attr, value)
           standardGeneric("nodeData<-"))


setGeneric("edgeData", function(self, from, to, attr)
           standardGeneric("edgeData"))


setGeneric("edgeData<-", function(self, from, to, attr, value)
           standardGeneric("edgeData<-"))
## ---------------------------------------------------------------------



## Basic operations

setGeneric("DFS", function(object, node, checkConn=FALSE) standardGeneric("DFS"))


setGeneric("subGraph", function(snodes, graph) standardGeneric("subGraph"))


setGeneric("intersection3", function(x, y) standardGeneric("intersection3"))


setGeneric("intersection2", function(x, y) standardGeneric("intersection2"))


setGeneric("intersection", function(x, y) standardGeneric("intersection"))


setGeneric("join", function(x, y) standardGeneric("join"))


setGeneric("union", function(x, y) standardGeneric("union"))


setGeneric("complement", function(x) standardGeneric("complement"))


setGeneric("connComp", function(object) standardGeneric("connComp"))


setGeneric("isConnected", function(object, ...) standardGeneric("isConnected"))


setGeneric("inEdges", function(node, object) standardGeneric("inEdges"))


setGeneric("edgeNames", function(object, ...) standardGeneric("edgeNames"))


setGeneric("isAdjacent", function(object, from, to, ...)
           standardGeneric("isAdjacent"))


## Generics for graph components


setGeneric("edgeID", function(object)
           standardGeneric("edgeID"))

setGeneric("eNode", function(object)
           standardGeneric("eNode"))

setGeneric("bNode", function(object)
           standardGeneric("bNode"))

setGeneric("toEdges", function(object) standardGeneric("toEdges"))

setGeneric("toEdges<-",
           function(object, value) standardGeneric("toEdges<-"))

setGeneric("fromEdges",
           function(object) standardGeneric("fromEdges"))

setGeneric("fromEdges<-",
           function(object, value) standardGeneric("fromEdges<-"))

setGeneric("label", function(object) standardGeneric("label"))

setGeneric("edgeOrder", function(object) standardGeneric("edgeOrder"))

setGeneric("nodeID", function(object) standardGeneric("nodeID"))

setGeneric("nodeType", function(object) standardGeneric("nodeType"))


## Misc

setGeneric("hash", function(key, value, htable)
           standardGeneric("hash"))


setGeneric("Dist", function(object) standardGeneric("Dist"))


setGeneric("threshold", function(object, k) standardGeneric("threshold"))


setGeneric("contents", function(object, all.names) standardGeneric("contents"))


setGeneric("edgeMatrix", function(object, duplicates=FALSE)
           standardGeneric("edgeMatrix"))


setGeneric("idstring", function(x) standardGeneric("idstring"))


setGeneric("edgeL", function(graph, index) standardGeneric("edgeL"))


setGeneric("clusteringCoefficient", function(object, ...)
           standardGeneric("clusteringCoefficient"))


## Generics for GXL

setGeneric("fromGXL", function(con) standardGeneric("fromGXL"))
setGeneric("dumpGXL", function(con) standardGeneric("dumpGXL"))
setGeneric("validateGXL", function(con) standardGeneric("validateGXL"))
setGeneric("toGXL", function(object) standardGeneric("toGXL"))


setGeneric("property", function(x, prop) standardGeneric("property"))


setGeneric("property<-", function(x, prop, value)
           standardGeneric("property<-"))



setGeneric("asGraphProperty", function(x, hasWeight)
           standardGeneric("asGraphProperty"))


##setGeneric("grList",function(object)standardGeneric("grList"))
##setGeneric("between",function(object)standardGeneric("between"))
##setGeneric("adjMat",function(cg,ordvec)standardGeneric("adjMat"))

setGeneric("toDotR", function(G, outDotFile, renderList, optList)
           standardGeneric("toDotR"))


