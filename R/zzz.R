
.onLoad <- function(libname, pkgname) {
    if (!require(methods))
       print("Error on Load: methods package not able to load")
    .initGContents()

 if((.Platform$OS.type == "windows") && ("Biobase" %in% installed.packages()[,"Package"])
    && (interactive()) && (.Platform$GUI ==  "Rgui")){
     if (require("Biobase"))
         addVigs2WinMenu("graph")
 }

}

