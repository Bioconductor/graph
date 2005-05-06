.onLoad <- function(libname, pkgname) {
    if (!require("methods"))
       stop("Unable to load ", sQuote("methods"), " package")
    .initGContents()

 if((.Platform$OS.type == "windows") && ("Biobase" %in% installed.packages()[,"Package"])
    && (interactive()) && (.Platform$GUI ==  "Rgui")){
     if (require("Biobase"))
         addVigs2WinMenu("graph")
 }

}

