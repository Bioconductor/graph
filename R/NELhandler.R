
NELhandler <- function() {
#
# handler for xmlEventParse for the node and edge
# components of GXL 1.0.1
#
#
# local store
#
     graphID <- NULL
     curNode <- NULL
     curEdge <- NULL
     curAttr <- NULL
     inNode <- FALSE
     inEdge <- FALSE
     inAttr <- FALSE
     nodeL <- list()
     edgeL <- list()
#
# handler elements: start elements are cased for
#  graph, node, attr, or edge
# text is limited in the simple example to the attr tag,
#   which lives under a node or an edge
#
     startElement = function(x, atts, ...) {
      if (x == "graph") graphID <<- atts["id"]
      else if (x == "node") {
       inNode <<- TRUE
       nodeL[[ atts["id"] ]] <<- list()
       curNode <<- atts["id"]
      }
      else if (x == "attr") {
       inAttr <<- TRUE
       curAttr <<- atts["name"]
      }
      else if (x == "edge") {
       inNode <<- FALSE
       inEdge <<- TRUE
       edgeL[[ atts["id"] ]] <<- list()
       edgeL[[ atts["id"] ]][["span"]] <<- c(atts["from"],
                  atts["to"])
       curEdge <<- atts["id"] 
      }
     }
     text = function(x, atts, ...) {
       if (inNode & inAttr & nchar(x)>0) 
              nodeL[[ curNode ]][[ curAttr ]] <<- x
       else if (inEdge & inAttr & nchar(x)>0) 
              edgeL[[ curEdge ]][[ curAttr ]] <<- x
     }
     endElement = function(x, ...) {
         if (x == "attr") inAttr <<- FALSE
         else if (x == "node") inNode <<- FALSE
         else if (x == "edge") inEdge <<- FALSE
     }
     dump = function() {
        list(graphID=graphID, nodeL=nodeL, edgeL=edgeL)
     }
     asGraphNEL = function() {
       require(graph)
       src <- sapply(edgeL,function(x)x$span["from"])
       dest <- sapply(edgeL,function(x)x$span["to"]) 
       ns <- names(nodeL)
       bas <- match(src,ns)
       tar <- match(dest,ns)
       edl <- list()
       for (i in 1:length(ns)) edl[[ns[i]]] <- list(edges=tar[bas==i])
       new("graphNEL", nodes=ns, edgeL=edl, edgemode="directed")
     }
     list( startElement=startElement, endElement=endElement,
                text=text, dump=dump, asGraphNEL=asGraphNEL )
   }
