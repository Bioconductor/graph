
.First.lib <- function(libname, pkgname, where) {

    ##load time code
    where = match(paste("package:", pkgname, sep=""), search())
    .initGContents(where)

    if(.Platform$OS.type == "windows" && require("Biobase") && interactive()
        && .Platform$GUI ==  "Rgui"){
        addVigs2WinMenu("graph")
    }
}
