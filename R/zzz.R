.First.lib <- function(libname, pkgname, where) {
    if( !require(methods)) stop("can only load graph with methods")
    require("Biobase")
    where <- match(paste("package:", pkgname, sep=""), search())
    .initGraph(where)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initGclass(where)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initDistGraph(where)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initClustGraph(where)
    where <- match(paste("package:", pkgname, sep=""), search())
    .GXLformals(where)
}
