.First.lib <- function(libname, pkgname, where) {
    ##require(Ruuid, quietly=TRUE)
    require(XML, quietly=TRUE)
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
