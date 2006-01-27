graphNELhandler <- function () 
{
    ##
    ## this function is to work with omegahat's XML xmlEventParse
    ## current version: given a GXL graph, capture the node names and
    ## edge data to return the graph as graph::graphNEL
    ##
    graphID <- NULL
    curNode <- NULL
    curEdge <- NULL
    curAttr <- NULL
    inNode <- FALSE
    inEdge <- FALSE
    inAttr <- FALSE
    inInt <- FALSE
    inFloat <- FALSE
    inBool <- FALSE
    g <- new("graphNEL")
    nodeL <- list()
    edgeL <- list()
    edgemode <- NULL
    ##
    ## handler elements: start elements are cased for
    ##  graph, node, attr, or edge
    ## text is limited in the simple example to the attr tag,
    ##   which lives under a node or an edge
    ##
    startElement <- function(x, atts, ...) {
        if (x == "graph") {
            graphID <<- atts["id"]
            eMode <- atts["edgemode"]
            if (!is.na(eMode)) {
                if (eMode %in% c("undirected", "defaultundirected"))
                  edgemode(g) <<- "undirected"
                else
                  edgemode(g) <<- "directed"
                ## not sure we'll need this
            } else {  ## default is directed for GXL
                edgemode(g) <<- "directed"
            }
            edgemode <<- atts["edgemode"]
        }
        else if (x == "node") {
            inNode <<- TRUE
            theNode <- as.character(atts["id"])
            if (! (theNode %in% nodes(g)))
              g <<- addNode(theNode, g) 
            nodeL[[theNode]] <<- list()
            curNode <<- theNode
        }
        else if (x == "attr") {
            inAttr <<- TRUE
            curAttr <<- atts["name"]
        }
        else if (x == "edge") {
            inNode <<- FALSE
            inEdge <<- TRUE
            from <- as.character(atts["from"])
            to <- as.character(atts["to"])
            if (!(from %in% nodes(g)))
              g <<- addNode(from, g)
            if (!(to %in% nodes(g)))
              g <<- addNode(to, g)
            g <<- addEdge(from=from,
                          to=to,
                          g)
            edgeL[[atts["id"]]] <<- list()
            edgeL[[atts["id"]]][["span"]] <<- c(from, to)
            curEdge <<- list(from=from, to=to)
        }
        else if (x == "int") {
            inInt <<- TRUE
        }
        else if (x == "float") {
            inFloat <<- TRUE
        }
        else if (x == "bool") {
            inBool <<- TRUE
        }
    }
    
    text <- function(x, atts, ...) {
        if (inAttr && nchar(x) > 0) {
            if (inInt) x <- as.integer(x)
            if (inFloat) x <- as.double(x)
            if (inBool) {
                if (identical(x, "true"))
                  x <- TRUE
                else if (identical(x, "false"))
                  x <- FALSE
                else
                  stop("bad bool value: ", x)
            }
            if (inNode) {
                if (!(curAttr %in% nodeDataDefaults(g)))
                  nodeDataDefaults(g, curAttr) <<- NA
                nodeData(g, curNode, curAttr) <<- x
            } else if (inEdge) {
                if (!(curAttr %in% edgeDataDefaults(g)))
                  edgeDataDefaults(g, curAttr) <<- NA
                edgeData(g, from=curEdge$from, to=curEdge$to, curAttr) <<- x
            }
        }
        
##         if (inNode && inAttr && nchar(x) > 0) {
##             if (!(curAttr %in% nodeDataDefaults(g)))
##               nodeDataDefaults(g, curAttr) <<- NA
##             if (inInt) x <- as.integer(x)
##             if (inFloat) x <- as.double(x)
##             nodeData(g, curNode, curAttr) <<- x
##         } else if (inEdge && inAttr && nchar(x) > 0) {
##             if (!(curAttr %in% edgeDataDefaults(g)))
##               edgeDataDefaults(g, curAttr) <<- NA
##             if (inInt) x <- as.integer(x)
##             if (inFloat) x <- as.double(x)
##             edgeData(g, from=curEdge$from, to=curEdge$to, curAttr) <<- x
##         }
    }
    endElement <- function(x, ...) {
        if (x == "attr") 
          inAttr <<- FALSE
        else if (x == "node") 
          inNode <<- FALSE
        else if (x == "edge") 
          inEdge <<- FALSE
        else if (x == "int")
          inInt <<- FALSE
        else if (x == "float")
          inFloat <<- FALSE
        else if (x == "bool")
          inBool <<- FALSE
    }
    dump <- function() {
        list(graphID = graphID, nodeL = nodeL, edgeL = edgeL, 
             edgemode = edgemode)
    }
    asGraphNEL <- function() {
        if (!validGraph(g)) 
          stop("GXL did not define a valid graph package graphNEL object.\nMost likely there is a failure of reciprocity for edges in\nan undirected graph.  If there is a node for edge from A to B\nin an undirected graphNEL, there must also be an edge from B to A.")
	return(g)
    }
    list(startElement = startElement, endElement = endElement, 
         text = text, dump = dump, asGraphNEL = asGraphNEL)
}
