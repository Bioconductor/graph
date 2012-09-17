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
    g <- graphNEL()
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
        if (!validGraph(g)) {
            msg <- "GXL did not define a valid graph package graphNEL
                    object. Most likely there is a failure of
                    reciprocity for edges in an undirected graph.  If
                    there is a node for edge from A to B in an
                    undirected graphNEL, there must also be an edge
                    from B to A."
            stop(paste0(c("", strwrap(msg)), collapse="\n"))
        }



	return(g)
    }
    list(startElement = startElement, endElement = endElement, 
         text = text, dump = dump, asGraphNEL = asGraphNEL)
}

graph_handler <- function ()
{
    ##
    ## this function is to work with omegahat's XML xmlEventParse
    ## current version: given a GXL graph, capture the node names and
    ## edge data to return the graph as graph::graphNEL
    ##
    all_nodes_e <- new.env(parent=emptyenv(), hash=TRUE)
    node_data_e <- new.env(parent=emptyenv(), hash=TRUE)
    node_defaults_e <- new.env(parent=emptyenv(), hash=TRUE)
    edge_data_e <- new.env(parent=emptyenv(), hash=TRUE)
    edge_defaults_e <- new.env(parent=emptyenv(), hash=TRUE)
    from_e <- new.env(parent=emptyenv(), hash=TRUE)
    to_e <- new.env(parent=emptyenv(), hash=TRUE)
    nodeCount <- 0L
    edgeCount <- 0L

    graphID <- NULL
    curNode <- NULL
    curAttr <- NULL
    inNode <- FALSE
    inEdge <- FALSE
    inAttr <- FALSE
    inInt <- FALSE
    inFloat <- FALSE
    inBool <- FALSE
    edgemode <- NULL

    add_node <- function(theNode) {
        if (!exists(theNode, all_nodes_e)) {
            nodeCount <<- nodeCount + 1L
            all_nodes_e[[theNode]] <- nodeCount
        }
    }

    add_edge <- function(from, to) {
        edgeCount <<- edgeCount + 1L
        ## FIXME: check for dup edge?
        k <- as.character(edgeCount)
        from_e[[k]] <- from
        to_e[[k]] <- to
    }

    ##
    ## handler elements: start elements are cased for
    ##  graph, node, attr, or edge
    ## text is limited in the simple example to the attr tag,
    ##   which lives under a node or an edge
    ##
    startElement <- function(x, atts, ...) {
        if (x == "graph") {
            if (!is.null(graphID))
              stop("multiple graphs not supported")
            graphID <<- atts["id"]
            eMode <- atts["edgemode"]
            if (!is.na(eMode)) {
                if (eMode %in% c("undirected", "defaultundirected"))
                  edgemode <<- "undirected"
                else
                  edgemode <<- "directed"
                ## not sure we'll need this
            } else {  ## default is directed for GXL
                edgemode <<- "directed"
            }
        }
        else if (x == "node") {
            inNode <<- TRUE
            theNode <- as.character(atts["id"])
            add_node(theNode)
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
            add_node(from)
            add_node(to)
            add_edge(from, to)
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
                node_defaults_e[[curAttr]] <- as.character(NA)
                nattrs <- node_data_e[[curNode]]
                if (!length(nattrs))
                  nattrs <- list()
                nattrs[[curAttr]] <- x
                node_data_e[[curNode]] <- nattrs
            } else if (inEdge) {
                edge_defaults_e[[curAttr]] <- as.character(NA)
                k <- as.character(edgeCount)
                eattrs <- edge_data_e[[k]]
                if (!length(eattrs))
                  eattrs <- list()
                eattrs[[curAttr]] <- x
                edge_data_e[[k]] <- eattrs
            }
        }
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
    asGraphNEL <- function() {
        ftmat <- cbind(from=unlist(as.list(from_e)),
                       to=unlist(as.list(to_e)))
        ## could call ftM2graphNEL here, but building up the object this
        ## way may be better... as we add the edges last.  Note that
        ## ftM2graphNEL is much pickier about duplicated edges for
        ## undirected graphs, so we would need to filter those for the
        ## undirected case.
        nn <- unlist(as.list(all_nodes_e))      # retain original node order
        nn <- names(nn)[order(nn)]
        g <- graphNEL(nodes=nn, edgemode=edgemode)
        if (length(node_defaults_e)) {
            nd <- new("attrData", as.list(node_defaults_e))
            nd@data <- as.list(node_data_e)
            g@nodeData <- nd
        }
        if (length(edge_data_e)) {
            ed <- new("attrData", as.list(edge_defaults_e))
            edvals <- as.list(edge_data_e)
            names(edvals) <- .makeEdgeKeys(ftmat[, 1], ftmat[, 2])
            ed@data <- edvals
            g@edgeData <- ed
        }
        g <- addEdge(ftmat[, 1], ftmat[, 2], g)
        validObject(g)
        g
    }
    list(startElement = startElement, endElement = endElement,
         text = text, asGraphNEL = asGraphNEL)
}
