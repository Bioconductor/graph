#### ---------------------------------------------
#### In base/R/packages.R
package.dependencies <- function(x, check = FALSE,
                                 depLevel=c("Depends", "Suggests"))
{
    depLevel <- match.arg(depLevel)

    if(!is.matrix(x))
        x <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))

    deps <- list()
    for(k in 1:nrow(x)){
        z <- x[k, depLevel]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ",", fixed=TRUE))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)

            ## split into package names and version
            pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
            deps[[k]] <-
                cbind(sub(pat, "\\1", z), sub(pat, "\\2", z), NA)

            noversion <- deps[[k]][,1] == deps[[k]][,2]
            deps[[k]][noversion,2] <- NA

            ## split version dependency into operator and version number
            pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
            deps[[k]][!noversion, 2:3] <-
                c(sub(pat, "\\1", deps[[k]][!noversion, 2]),
                  sub(pat, "\\2", deps[[k]][!noversion, 2]))
        }
        else
            deps[[k]] <- NA
    }

    if(check){
        z <- rep.int(TRUE, nrow(x))
        for(k in 1:nrow(x)){
            ## currently we only check the version of R itself
            if(!is.na(deps[[k]]) &&
               any(ok <- deps[[k]][,1] == "R")) {
                ## NOTE: currently operators must be `<=' or `>='.
                if(!is.na(deps[[k]][ok, 2])
                   && deps[[k]][ok, 2] %in% c("<=", ">=")) {
                    comptext <-
                        paste('"', R.version$major, ".",
                              R.version$minor, '" ',
                              deps[[k]][ok,2], ' "',
                              deps[[k]][ok,3], '"', sep = "")
                    compres <- try(eval(parse(text = comptext)))
                    if(!inherits(compres, "try-error"))
                        z[k] <- compres
                }
            }
        }
        names(z) <- x[,"Package"]
        return(z)
    }
    else{
        names(deps) <- x[,"Package"]
        return(deps)
    }
}
#### -------------------------------------------------------------------

pkgInstOrder <- function(pkg, repGraph) {
    require("RBGL") || stop("RBGL needed for pkgInstOrder")


    if (missing(pkg))
        stop("No package argument provided")

    if (length(pkg) != 1)
        stop("package parameter must be of length 1")

    ## dijkstra.sp requires type graphNEL
    if (! inherits(repGraph, "graphNEL"))
        stop("repGraph must be of type graph")

    if (! pkg %in% nodes(repGraph))
        stop("pkg parameter not a node in provided repGraph")

    dists <- dijkstra.sp(repGraph, pkg)$distances
    instDists <- dists[dists != Inf]

    if (length(instDists) > 0)
        instOrder <- names(rev(sort(instDists)))
    else
        character()
}

buildRepDepGraph <- function(repository, depLevel=c("Depends", "Suggests")) {
    require("graph") || stop("buildRepDepGraph needs package 'graph'")

    depLevel <- match.arg(depLevel)

    if (missing(repository))
        stop("No online repositories specified")
    if (length(repository) != 1)
        stop("The repository argument must be of length 1")

    dGraph <- new("graphNEL", edgemode="directed")

    cran <- CRAN.packages(contriburl=repository)
    pkgDeps <- package.dependencies(cran, check=FALSE, depLevel=depLevel)
    pkgs <- names(pkgDeps)
    for (i in seq(along=pkgDeps)) {
        if (! pkgs[i] %in% nodes(dGraph))
            dGraph <- addNode(pkgs[i], dGraph)
        curMtrx <- pkgDeps[[i]]

        if (! is.matrix(curMtrx))
            next

        for (j in seq(along=curMtrx[,1])) {
            depPkg <- curMtrx[j,1]
            if (depPkg == "R")
                next

            if (! depPkg %in% nodes(dGraph))
                dGraph <- addNode(depPkg, dGraph)

            dGraph <- addEdge(pkgs[i], depPkg, dGraph, 1)
        }
    }

    dGraph
}

