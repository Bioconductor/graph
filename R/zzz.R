.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load ", sQuote("methods"), " package")
    .initGContents()


}

.onAttach <- function(libname, pkgname) {
 if ((.Platform$OS.type == "windows") && interactive()
     && (.Platform$GUI ==  "Rgui")) {
     if (suppressWarnings(require("Biobase")))
         addVigs2WinMenu("graph")
 }
}


.onUnload <- function( libpath ) {
  library.dynam.unload( "graph", libpath )
}
