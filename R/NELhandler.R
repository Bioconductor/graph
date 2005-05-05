NELhandler <- function () 
{
#
# this function is to work with omegahat's XML xmlEventParse
# current version: given a GXL graph, capture the node names and
# edge data to return the graph as graph::graphNEL
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
    edgemode <- NULL
#
# handler elements: start elements are cased for
#  graph, node, attr, or edge
# text is limited in the simple example to the attr tag,
#   which lives under a node or an edge
#
    startElement = function(x, atts, ...) {
        if (x == "graph") {
            graphID <<- atts["id"]
            edgemode <<- atts["edgemode"]
        }
        else if (x == "node") {
            inNode <<- TRUE
            nodeL[[atts["id"]]] <<- list()
            curNode <<- atts["id"]
        }
        else if (x == "attr") {
            inAttr <<- TRUE
            curAttr <<- atts["name"]
        }
        else if (x == "edge") {
            inNode <<- FALSE
            inEdge <<- TRUE
            edgeL[[atts["id"]]] <<- list()
            edgeL[[atts["id"]]][["span"]] <<- c(atts["from"], 
                atts["to"])
            curEdge <<- atts["id"]
        }
    }
    text = function(x, atts, ...) {
        if (inNode & inAttr & nchar(x) > 0) 
            nodeL[[curNode]][[curAttr]] <<- x
        else if (inEdge & inAttr & nchar(x) > 0) 
            edgeL[[curEdge]][[curAttr]] <<- c(edgeL[[curEdge]][[curAttr]], 
                x)
    }
    endElement = function(x, ...) {
        if (x == "attr") 
            inAttr <<- FALSE
        else if (x == "node") 
            inNode <<- FALSE
        else if (x == "edge") 
            inEdge <<- FALSE
    }
    dump = function() {
        list(graphID = graphID, nodeL = nodeL, edgeL = edgeL, 
            edgemode = edgemode)
    }
    asGraphNEL = function() {
#
# revised Jun 16 2004
# when callsed, nodeL is the named list of node data, edgeL is
# named list of edge data (unrelated to edgeL of graphNEL!!!)
#
        require(graph)
        ns <- names(nodeL)
        if (length(edgeL) == 0) 
            return(new("graphNEL", nodes = ns, edgemode = edgemode))
#
# edgeL has a span element giving source and destination of each
# edge
#
        src <- sapply(edgeL, function(x) x$span["from"])
        dest <- sapply(edgeL, function(x) x$span["to"])
        wts <- sapply(edgeL, function(x) as.numeric(x$weights))
        NOWTS <- FALSE
        if (all(sapply(wts,length)==0)) NOWTS <- TRUE
        names(wts) <- dest
#
# graphNEL edgeL structure is a named list with one element
# for each node of graph.  the edges component for a node N
# has node indices of the destinations of each edge starting at N
#
        desti <- match(dest, ns)
        edl <- split(desti, src)
        wtl <- split(wts, src)
        for (i in 1:length(ns)) {
          if (length(edl[[ns[i]]]) == 0)
            edl[[ns[i]]] <- list(edges = integer(0))
          else if (!NOWTS)
            edl[[ns[i]]] <- list(edges = edl[[ns[i]]], weights=wtl[[ns[i]]])
          else
            edl[[ns[i]]] <- list(edges = edl[[ns[i]]])
        }
        edl <- edl[ns]
        g <- new("graphNEL", nodes = ns, edgeL = edl, edgemode = edgemode)
        if (edgemode(g) == "undirected") 
            {
            edgemode(g) <- "directed" # allow ugraph to do something
            g <- ugraph(g)
            edgemode(g) <- "undirected"
            }
        if (!validGraph(g)) 
            stop("GXL did not define a valid graph package graphNEL object.\nMost likely there is a failure of reciprocity for edges in\nan undirected graph.  If there is a node for edge from A to B\nin an undirected graphNEL, there must also be an edge from B to A.")
	return(g)
    }
    list(startElement = startElement, endElement = endElement, 
        text = text, dump = dump, asGraphNEL = asGraphNEL)
}

