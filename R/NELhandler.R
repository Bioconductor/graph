NELhandler <- function () 
{
#
# handler for xmlEventParse for the node and edge
# components of GXL 1.0.1
#  REVISED 1 apr 03 so that weights come into graphNEL if
#   present as weights child of edges in GXL
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
        if (x == "graph") 
            graphID <<- atts["id"]
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
#
# you should get rid of the dependence on atts["id"] to allow edgeids=FALSE GXL to succeed
# consider an automatic edge labeler
#
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
        list(graphID = graphID, nodeL = nodeL, edgeL = edgeL)
    }
    asGraphNEL = function() {
#
# just handles weights children of edges when present, needs to handle
# arbitrary children of edges and nodes
#
        require(graph)
        ns <- names(nodeL)
        if (length(edgeL) == 0)
        return(new("graphNEL", nodes = ns, edgemode = "directed"))
        src <- sapply(edgeL, function(x) x$span["from"])
        dest <- sapply(edgeL, function(x) x$span["to"])
        wts <- sapply(edgeL, function(x) x[["weights"]])
        IGNWTS <- FALSE
        if (any(sapply(wts,function(x)is.null(x)))) IGNWTS <- TRUE
        if (!IGNWTS) sw <- split(wts, src)
        edl <- split(dest, src)
        nne <- names(edl)
        nl <- list()
        if (!IGNWTS) for (i in 1:length(edl)) nl[[nne[i]]] <- list(edges = match(edl[[i]], 
            ns), weights = as.numeric(sw[[i]]))
        else for (i in 1:length(edl)) nl[[nne[i]]] <- list(edges = match(edl[[i]], 
            ns))
	chkENconsis <- match(ns, names(nl))
        if (any(inn <- is.na(chkENconsis)))
		{
		badinds <- (1:length(chkENconsis))[inn==TRUE]
		for (i in 1:length(badinds))
			nl[[ ns[ badinds[i] ] ]] <- character(0)
		}
        new("graphNEL", nodes = ns, edgeL = nl, edgemode = "directed")
    }
    list(startElement = startElement, endElement = endElement, 
        text = text, dump = dump, asGraphNEL = asGraphNEL)
}
