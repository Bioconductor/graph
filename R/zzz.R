.First.lib <- function(libname, pkgname, where) {
    ##require(Ruuid, quietly=TRUE)
    require(XML, quietly=TRUE)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initGraph(as.environment(where))
    where <- match(paste("package:", pkgname, sep=""), search())
    .initGclass(as.environment(where))
    where <- match(paste("package:", pkgname, sep=""), search())
    .initDistGraph(as.environment(where))
    where <- match(paste("package:", pkgname, sep=""), search())
    .initClustGraph(as.environment(where))
    where <- match(paste("package:", pkgname, sep=""), search())
    .GXLformals(as.environment(where))
}
