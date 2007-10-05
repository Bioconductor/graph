


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
