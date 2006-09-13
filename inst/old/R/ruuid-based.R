setClass("graphH",
         representation(graphID="Ruuid",
                        nodes="list",
                        label2nodeID="character",
                        edges="environment"),
         contains="graph")

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


## Graph components
## Not currently being used...

setClass("propertyHolder", representation(property="list"),
         contains="VIRTUAL")



setClass("propertySet", representation(data="environment"))

setClass("hashtable", representation(hashtable="environment"))


setClass("generalGraph", representation(nodes="hashtable",
                                        edges="hashtable"),
         contains="graph")

