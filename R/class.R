##Copyright 2002 Robert Gentleman, all rights reserved

##a class structure for graphs
##the class structure is based on GXL

.initGclass <- function(where) {
    setClass("gNode",
           representation(nodeID="integer",
                          nodeType="character",
                          toEdges="integer",
                          fromEdges="integer",
                          edgeOrder="integer",
                          label="character" ),
           prototype = list(nodeID=-1, nodetype="unknown",
             edgeOrder=0, label=""),
           where=where)

    setClass("gEdge",
          representation(edgeID="integer",
                         edgeType="character",
                         weight="numeric",
                         bNode="integer",    #begin
                         eNode="integer"),   #end
          prototype = list(id=-1, type="unkown", begin=-1, end=-1),
          where = where)

   setGeneric("toEdges", function(object) standardGeneric("toEdges"),
   where=where)

   setMethod("toEdges", "gNode", function(object) object@toEdges, where=where)

   setGeneric("toEdges<-",
               function(object, value) standardGeneric("toEdges<-"),
               where = where)
   setReplaceMethod("toEdges", "gNode", function(object, value) {
      object@toEdges <- value
      object}, where=where)

    setGeneric("fromEdges",
               function(object) standardGeneric("fromEdges"), 
               where=where)
   setMethod("fromEdges", "gNode", function(object) object@fromEdges, 
             where=where)
    
    setGeneric("fromEdges<-", 
               function(object, value) standardGeneric("fromEdges<-"),
               where = where)
     setReplaceMethod("fromEdges", "gNode", function(object, value) {
      object@fromEdges <- value
      object}, where = where)

      setGeneric("label", function(object) standardGeneric("label"),
           where=where)
   setMethod("label", "gNode", function(object) object@label,
     where = where)

     setGeneric("edgeOrder", function(object) standardGeneric("edgeOrder"),
   where=where)
   setMethod("edgeOrder", "gNode", function(object) object@edgeOrder, 
     where = where)

    setGeneric("nodeID", function(object) standardGeneric("nodeID"),
               where=where)
    
   setMethod("nodeID", "gNode", function(object) object@nodeID, 
     where = where) 

    setGeneric("nodeType", function(object) standardGeneric("nodeType"),
               where=where)
    
   setMethod("nodeType", "gNode", function(object) object@nodeType, 
     where = where) 

} 


