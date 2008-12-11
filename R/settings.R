## change or return the current defaults for a graph's rendering information
graph.par <- function(...)
{
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]]))
        new <- new[[1]]
    old <- .GraphEnv$par

    ## if no args supplied, returns full par list
    if (length(new) == 0) return(old)
    ## typically getting par
    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)])

    ## setting at least one par, but may get some as well (unlikely in practice)
    isNamed <- nm != ""
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])
    ## so now everything has non-"" names, but only the isNamed ones
    ## should be set.  Everything should be returned though.
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    .GraphEnv$par <- modifyList(old, new[nm])
    invisible(retVal)
}


## get a particular graph rendering parameter set. Valid sets are "nodes",
## "edges" and "graph"
graph.par.get <- function(name) .GraphEnv$par[[name]]


## need NULL or empty string for everything that should not be set to
## allow for resetting (like labels and title)
.default.graph.pars <- function()
    list(nodes =
         list(col = "black", fill = "transparent",
              textCol = "black", fontsize=14,
              lty = 1, lwd = 1, label=NULL,
              fixedsize=FALSE, shape="circle",
              iwidth=0.75, iheight=0.5),
         edges =
         list(col = "black", lty = 1, lwd = 1,
              textCol = "black", cex = 1,
              fontsize=14),
         graph =
         list(laidout=FALSE, recipEdges="combined", main="", sub="",
              cex.main=1.2, cex.sub=1, label=NULL,
              col.main="black", col.sub="black"))


## create a renderInfo object
.renderInfoPrototype <- new("renderInfo")


## FIXME: make these generic?
## return node-specific rendering parameters 
nodeRenderInfo <- function(g, name)
{
    if(missing(name))
        g@renderInfo@nodes
    else{
        tmp <- g@renderInfo@nodes[name]
        if(length(tmp)==1)
            tmp <- tmp[[1]]
        tmp
    }
}


## return edge-specific rendering parameters 
edgeRenderInfo <- function(g, name)
{
    if(missing(name))
        g@renderInfo@edges
    else{ 
        tmp <- g@renderInfo@edges[name]
        if(length(tmp)==1)
            tmp <- tmp[[1]]
        tmp
    }
}


## return graph-specific rendering parameters 
graphRenderInfo <- function(g, name)
{
    if(missing(name))
        g@renderInfo@graph
    else{
        tmp <- g@renderInfo@graph[name]
        if(length(tmp)==1)
            tmp <- tmp[[1]]
        tmp
    }
}

## return content of the pars slot
parRenderInfo <- function(g, name)
{
    if(missing(name))
        g@renderInfo@pars
    else
        g@renderInfo@pars[[name]]
}



## changes renderInfo settings of a graph g
setRenderInfo <- function(g, what, value, validNames, n = length(validNames))
{
    ## FIXME: what's supposed to happen if graph is not already laid out?
    if (!is.list(value) || is.null(names(value)) || any(!nzchar(names(value))))
        stop("'value' must be a list of named parameters")
    for (i in names(value))
    {
        thisVal <- value[[i]]
        if (is.null(slot(g@renderInfo, what)[[i]]))
        {
            ## i doesn't exist.  Need to create appropriate placeholder
            m <- if(is.null(thisVal)) "character" else mode(thisVal)
            slot(g@renderInfo, what)[[i]] <-
                vector(mode=m, length = n)
            ## initialize to NA (seems to work for lists too, but may
            ## need methods for non-trivial objects)
            is.na(slot(g@renderInfo, what)[[i]]) <- TRUE
            names(slot(g@renderInfo, what)[[i]]) <- validNames
        }
        ## Now replace relevant parts
        if (length(thisVal) <= 1 && is.null(names(thisVal)))
        {
            ## change everything or revert to default if value is NULL
            repl <-
                if(is.null(thisVal)) graph.par()[[what]][[i]] else thisVal
            if(is.null(repl)){
                slot(g@renderInfo, what)[[i]] <- repl
            }else{
                for(j in seq_along(slot(g@renderInfo, what)[[i]]))
                    slot(g@renderInfo, what)[[i]][[j]] <- repl
            }
        }
        else
        {
            ## change only named values
            ## FIXME: check for all(names(thisVal) %in% nms) ?
            repNames <- intersect(names(thisVal), validNames)
            null <- sapply(thisVal, is.null)
            if(any(!null))
                slot(g@renderInfo, what)[[i]][repNames][!null] <-
                    thisVal[intersect(repNames, names(which(!null)))]
            if(any(null))
                 slot(g@renderInfo, what)[[i]][intersect(repNames, names(which(null)))] <-
                     graph.par()[[what]][[i]]
        }
    }
    g
}


## setter for node render parameters
"nodeRenderInfo<-" <- function(g, value)
{
    suppressWarnings(setRenderInfo(g, what = "nodes", value = value,
                                   validNames = nodes(g)))
}


## swap tail and head in edge names
swapNames <- function(names){
    if(!is.null(names)){
        ns <- strsplit(names, "~")
        sapply(ns, function(x) paste(x[2], x[1], sep="~"))
    }
}


## setter for edge render parameters
"edgeRenderInfo<-" <- function(g, value)
{
    ## edge tail and head order doesn't matter for undirected graphs
    ## so we simply duplicate the settings and swap tails and heads
    ## in the duplicate's names
    if(!isDirected(g)){
        value <- lapply(value, function(x){
            y <- x
            if(length(x)>1 && !is.null(names(x))){
                y <- rep(x,2)
                names(y) <- c(names(x), swapNames(names(x)))
            }
            y
        })
    }               
    suppressWarnings(setRenderInfo(g, what = "edges", value = value,
                  validNames=edgeNames(g,
                  recipEdges=graphRenderInfo(g, "recipEdges"))))
}


## setter for graph render parameters
"graphRenderInfo<-" <- function(g, value)
{
    ## value may be a arbitrary list
    if (!is.list(value))
        stop("'value' must be a list")
    g@renderInfo@graph <- suppressWarnings(modifyList(g@renderInfo@graph,
                                                      value))
    g
}


## setter for the pars slot
"parRenderInfo<-" <- function(g, value)
{
    ## value may be a list with components nodes, edges (like graph.pars())
    if (!is.list(value) || !names(value) %in% c("nodes", "edges", "graph"))
        stop("'value' must be a list, with possible components named ",
             "'nodes', 'edges' and 'graph'")
    if (any(unlist(lapply(value, function(x) sapply(x, length))) > 1))
        stop("all components of 'value$nodes', 'value$edges' and ",
             "'value$graph' must have length 1")
    g@renderInfo@pars <- suppressWarnings(modifyList(g@renderInfo@pars, value))
    g
}


