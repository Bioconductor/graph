pkgInstOrder <- function(pkg, repGraph) {
    require("RBGL", quietly=TRUE) || stop("RBGL needed for pkgInstOrder")


    if (missing(pkg))
        stop("No package argument provided")

    if (length(pkg) != 1)
        stop("package parameter must be of length 1")


    if (! is(repGraph, "graphNEL"))
        stop("repGraph must be of type graph")

    nodes <- nodes(repGraph)

    if (! pkg %in% nodes) {
        warning("pkg ", pkg, " not a node in provided repGraph")
        pkg
    }
    else {
        finishTime <- dfs(repGraph)$finish
        nodes <- nodes[finishTime]

        ## Now need to get only the nodes that are downstream from the pkg
        downstream <- names(acc(repGraph, pkg))

        if (length(downstream) > 0) {
            order <- match(nodes, downstream)
            order <- order[!is.na(order)]
            c(downstream[order], pkg)
        }
        else
            pkg
    }
}

buildRepDepGraph <- function(repository, depLevel=c("Depends",
                                         "Suggests")) {
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

