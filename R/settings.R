


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

graph.par.get <- function(name) .GraphEnv$par[[name]]

.default.graph.pars <- function()
    list(nodes =
         list(col = "black", fill = "transparent",
              textCol = "black", cex = 1,
              lty = 1, lwd = 1),
         edges =
         list(col = "black", lty = 1, lwd = 1,
              textCol = "black", cex = 1))




.renderInfoPrototype <- new("renderInfo")

## FIXME: make these generic?


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
        if (is.null(slot(g@renderInfo, what)[[i]]))
        {
            ## i doesn't exist.  Need to create appropriate placeholder
            slot(g@renderInfo, what)[[i]] <-
                vector(mode = mode(value[[i]]), length = n)
            ## initialize to NA (seems to work for lists too, but may
            ## need methods for non-trivial objects)
            is.na(slot(g@renderInfo, what)[[i]]) <- TRUE
            names(slot(g@renderInfo, what)[[i]]) <- validNames
        }
        ## Now replace relevant parts
        if (length(value[[i]]) == 1 && is.null(names(value[[i]])))
        {
            ## change everything
            slot(g@renderInfo, what)[[i]][ ] <- value[[i]]
        }
        else
        {
            ## change only named values
            ## FIXME: check for all(names(value[[i]]) %in% nms) ?
            repNames <- intersect(names(value[[i]]), validNames)
            slot(g@renderInfo, what)[[i]][repNames] <- value[[i]][repNames]
        }
    }
    g
}

"nodeRenderInfo<-" <- function(g, value)
{
    setRenderInfo(g, what = "nodes", value = value, validNames = nodes(g))
}

"edgeRenderInfo<-" <- function(g, value)
{
    setRenderInfo(g, what = "edges", value = value, validNames = edgeNames(g))
}

"parRenderInfo<-" <- function(g, value)
{
    ## value may be a list with components nodes, edges (like graph.pars())
    if (!is.list(value) || !names(value) %in% c("nodes", "edges"))
        stop("'value' must be a list, with possible components named 'nodes' and 'edges'")
    if (any(unlist(lapply(value, function(x) sapply(x, length))) > 1))
        stop("all components of 'value$nodes' and 'value$edges' must have length 1")
    g@renderInfo@pars <- modifyList(g@renderInfo@pars, value)
    g
}


