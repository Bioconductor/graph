## Generic methods that all graph representation classes must support

setGeneric("isDirected", function(object) standardGeneric("isDirected"))

## ---------------------------------------------------------------------
## Node and edge access
## ---------------------------------------------------------------------
## setGeneric("getNodes", function(x, which) standardGeneric("getNodes"))


setGeneric("edgemode", function(object) standardGeneric("edgemode"))


setGeneric("edgemode<-", function(object, value) standardGeneric("edgemode<-"))


setGeneric("nodes", function(object, ...) standardGeneric("nodes"))


setGeneric("nodes<-", function(object, value) standardGeneric("nodes<-"))

## internal use only so that nodes<- can be a template method
setGeneric("renameNodes", function(g, value) standardGeneric("renameNodes"))


setGeneric("edges", function(object, which, ...) standardGeneric("edges"))


## The funny arg=1, is to allow default values in the methods.
## We don't want to dispatch on those args, but we want them
## named as part of the interface, hence the trick of putting them
## after the (...).  Note that this means partial matching for those
## args will not work, must specify full name.
setGeneric("edgeWeights", function(object, index, ...,
                                   attr="weight", default=1,
                                   type.checker=is.numeric)
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


setGeneric("removeAttrDataItem<-", function(self, x, value)
           standardGeneric("removeAttrDataItem<-"))


## ---------------------------------------------------------------------
## graph, node, edge attribute generics
## ---------------------------------------------------------------------
setGeneric("graphData", function(self, attr)
           standardGeneric("graphData"))

setGeneric("graphData<-", function(self, attr, value)
           standardGeneric("graphData<-"))

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

setGeneric("DFS", function(object, node, checkConn=TRUE) standardGeneric("DFS"))


setGeneric("subGraph", function(snodes, graph) standardGeneric("subGraph"))


setGeneric("intersection2", function(x, y) standardGeneric("intersection2"))


setGeneric("intersection", function(x, y) standardGeneric("intersection"))


setGeneric("join", function(x, y) standardGeneric("join"))


setGeneric("union", function(x, y) standardGeneric("union"))

setGeneric("ugraph", function(graph) standardGeneric("ugraph"))

setGeneric("complement", function(x) standardGeneric("complement"))


setGeneric("connComp", function(object) standardGeneric("connComp"))


setGeneric("isConnected", function(object, ...) standardGeneric("isConnected"))


setGeneric("inEdges", function(node, object) standardGeneric("inEdges"))

setGeneric("leaves", signature="object", # don't dispatch on degree.dir
           function(object, degree.dir) standardGeneric("leaves"))


setGeneric("edgeNames", function(object, ...) standardGeneric("edgeNames"))


setGeneric("isAdjacent", function(object, from, to, ...)
           standardGeneric("isAdjacent"))

## Misc

setGeneric("Dist", function(object) standardGeneric("Dist"))


setGeneric("threshold", function(object, k, value=0) standardGeneric("threshold"))


setGeneric("contents", function(object, all.names) standardGeneric("contents"))


setGeneric("edgeMatrix", function(object, duplicates=FALSE)
           standardGeneric("edgeMatrix"))


setGeneric("edgeL", function(graph, index) standardGeneric("edgeL"))


setGeneric("clusteringCoefficient", function(object, ...)
           standardGeneric("clusteringCoefficient"))


## Generics for GXL

setGeneric("fromGXL", function(con) standardGeneric("fromGXL"))
setGeneric("dumpGXL", function(con) standardGeneric("dumpGXL"))
setGeneric("validateGXL", function(con) standardGeneric("validateGXL"))
setGeneric("toGXL", function(object, ...) standardGeneric("toGXL"))


setGeneric("property", function(x, prop) standardGeneric("property"))


setGeneric("toDotR", function(G, outDotFile, renderList, optList)
           standardGeneric("toDotR"))


## Updating a graph object
setGeneric("updateGraph", function(object)
           standardGeneric("updateGraph"))
