
.First.lib <- function(libname, pkgname, where) {

    if(.Platform$OS.type == "windows" && require("Biobase") && interactive()
        && .Platform$GUI ==  "Rgui"){
        addPDF2Vig("graph")
    }
}
