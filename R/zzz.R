.First.lib <- function(libname, pkgname, where) {
    if( !require(methods)) stop("can only load graph with methods")
    require("Biobase")
    where <- match(paste("package:", pkgname, sep=""), search())
    .initGraph(where)
    .initGclass(where)
    .initDistGraph(where)
    .initClustGraph(where)
    cacheMetaData(as.environment(where))
}
