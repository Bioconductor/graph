.invertListOfLists <- function(x) {
    if (length(x) == 0) {
        return(list())
    }
    numItems <- length(x[[1]])
    resList.inverted <- vector(mode="list", length=numItems)
    for (i in seq_len(numItems)) {
        resList.inverted[[i]] <- lapply(x, "[[", i=i)
    }
    names(resList.inverted) <- names(x[[1L]])
    resList.inverted
}

.wrapID <- function(id) {
    if (is.null(id) || id == "") {
        NULL
    } else {
        paste0("\"", id, "\"")
    }
}

.wrapAttrVal <- function(id) {
    id_names <- names(id)
    isHTML <- grepl("^<.*>$", id)

    ## escape the newlines
    id[!isHTML] <- paste0("\"", gsub("\n", "\\\n", id[!isHTML], fixed=TRUE),
                          "\"")
    names(id) <- id_names
    id
}

.splitRenderInfo <- function(renderInfo.list) {
    ## extract the attributes that are global
    attr_names <- names(renderInfo.list)
    global <- list()
    local <- list()
    for (attr_name in attr_names) {
        if (length(unique(renderInfo.list[[attr_name]])) == 1) {
            global[[attr_name]] <- unique(renderInfo.list[[attr_name]])
        } else {
            local[[attr_name]] <- renderInfo.list[[attr_name]]
        }
    }
    list(global=global, local=.invertListOfLists(local))
}

.splitNodeRenderInfo <- function(graph) {
    nri <- nodeRenderInfo(graph)
    .splitRenderInfo(nri)
}

.splitEdgeRenderInfo <- function(graph) {
    eri <- edgeRenderInfo(graph)
    .splitRenderInfo(eri)
}

.print_attr_list <- function(attr_list) {
    ## make the attribute list double-quoted strings;
    ## with escapted newline characters
    attr_vec <- unlist(attr_list, use.names=TRUE)
    if (length(attr_vec) == 0) {
        return("[]")
    }
    attr_vec <- attr_vec[!is.na(attr_vec)]
    attr_vec <- .wrapAttrVal(attr_vec)
    if (length(attr_vec) > 0) {
        foo <- paste(names(attr_vec), attr_vec, sep="=", collapse = ", ")
        paste0("[", foo, "]")
    } else {
        "[]"
    }
}

.print_node_stmt <- function(node_id, attr_list) {
    node_id <- .wrapID(node_id)
    paste( node_id, .print_attr_list(attr_list))
}

.print_edge_stmt <- function(edge_id, attr_list, edgeop) {
    ## name uses a tilde between nodes as teh edgename
    nodes <- strsplit(edge_id, "~", fixed=TRUE)[[1L]]
    nodes <- .wrapID(nodes)
    edge_id <- paste(nodes[1L], edgeop, nodes[2L])

    paste(edge_id, .print_attr_list(attr_list))
}

.print_attr_stmt <- function(type, attr_list) {
    paste(type, .print_attr_list(attr_list))
}

.print_stmt_list_fromGraph <- function(graph, edgeop) {
    ##print the graph, edges and nodes
    graph_attr_list <- graphRenderInfo(graph)
    node_RI_split <- .splitNodeRenderInfo(graph)
    edge_RI_split <- .splitEdgeRenderInfo(graph)

    ## print global graph attributes
    graph_printed <- .print_attr_stmt("graph", graph_attr_list)

    ## print global node attributes
    nodes_printed <- character(length(nodes(graph)) + 1L)
    nodes_printed[1L] <- .print_attr_stmt("node", node_RI_split$global)
    ## then ensure only renderInfo of present nodes is used locally
    ## and in correct order
    i <- 2L
    for (node_id in nodes(graph)) {
        nodes_printed[i] <-
            .print_node_stmt(node_id=node_id,
                             attr_list=node_RI_split$local[[node_id]])
        i <- i + 1L
    }

    ## print the global edge attributes
    if (edgeop == "->") { ## directed
        edge_names <- edgeNames(graph, recipEdges="distinct")
    } else {
        edge_names <- edgeNames(graph, recipEdges="combined")
    }
    edges_printed <- character(length(edge_names) + 1L)
    edges_printed[1L] <- .print_attr_stmt("edge", edge_RI_split$global)
    ## then ensure that only renderInfo of present edges is used and
    ## in correct order
    i <- 2L
    for (edge_id in edge_names) {
        edges_printed[i] <-
            .print_edge_stmt(edge_id=edge_id,
                             attr_list=edge_RI_split$local[[edge_id]],
                             edgeop=edgeop)
        i <- i + 1L
    }

    paste0(c(graph_printed, nodes_printed, edges_printed), ";")
}

.print_subgraph <- function(subgraph_id, subgraph, edgeop) {
    first_line <- paste("subgraph", .wrapID(subgraph_id), "{")
    last_line <- "}"
    stmt_list <- .print_stmt_list_fromGraph(subgraph, edgeop)
    c(first_line, stmt_list, last_line)
}

## see ?toDotWithRI
toDotWithRI <-
    function(graph, graph_name=NULL, subGraphList=list(), isStrict=TRUE)
{
    ## check for the correct class
    if (!inherits(graph, "graph")) {
        stop("graph has to inherit from class graph")
    }
    if (!all(unlist(lapply(subGraphList, inherits, what="graph")))) {
        stop("all elements in subGraphList must inherit from class graph")
    }
    
    if (isStrict) {
        strict_item <- "strict"
    } else {
        strict_item <- NULL
    }
    if (isDirected(graph)) {
        graph_type <- "digraph"
        edgeop <- "->"
    } else {
        graph_type <- "graph"
        edgeop <- "--"
    }

    printed_graph_stmts <- .print_stmt_list_fromGraph(graph, edgeop)
    
    printed_subgraph_list <- vector("list", length(subGraphList))
    for (i in seq_along(subGraphList)) {
        printed_subgraph_list[[i]] <-
            .print_subgraph(names(subGraphList)[i], subGraphList[[i]], edgeop)
    }

    c(paste(strict_item, graph_type, .wrapID(graph_name), "{"),
      printed_graph_stmts, unlist(printed_subgraph_list),
      "}")
}
