
.First.lib <- function(libname, pkgname, where) {
    library.dynam("graph", pkgname, libname)
    ##load time code
    where = match(paste("package:", pkgname, sep=""), search())
    .initGContents(where)

 if((.Platform$OS.type == "windows") && ("Biobase" %in% installed.packages()[,"Package"])
    && (interactive()) && (.Platform$GUI ==  "Rgui")){
     if (require("Biobase"))
         addVigs2WinMenu("graph")
 }

}
