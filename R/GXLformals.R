#
# GXL support
#
## need methods on connections
setIs("file","connection")
## fromGXL returns the graphNEL object only, and it may
##  need to return more properties (7 mar 03)

setMethod("fromGXL", signature(con="connection"),
          function(con) {
              require("XML") || stop("XML package needed")
              contents <- paste(readLines(con), collapse="")
              xmlEventParse(contents, graphNELhandler(),
                            asText=TRUE)$asGraphNEL()
          })


## dumpGXL returns an R list with all? properties

     setMethod("dumpGXL", "connection", function(con)
       {
       require("XML") || stop("XML package needed")
       xmlEventParse(paste(readLines(con),collapse=""),NELhandler(),asText=TRUE)$dump()
       })
## validate against the dtd

     setMethod("validateGXL", "connection", function(con)
       {
       require("XML") || stop("XML package needed")
# maybe need a try here, xmlTreeParse dumps the whole stream when it hits an error
       tmp <- xmlTreeParse(paste(readLines(con),collapse=""),asText=TRUE,
              validate=TRUE)
       })
#
#  exporting
#

    setMethod("toGXL", "graphNEL", function(object)
       gxlTreeNEL(object))


gxlTreeNEL <- function(gnel) {
    require("XML") || stop("XML package needed")
    nds <- nodes(gnel)
    eds <- lapply(edges(gnel),unique)
    enms <- names(eds)
    out <- xmlTree("gxl", #dtd="http://www.gupro.de/GXL/gxl-1.0.1.dtd",
                   namespaces=c(gxl="http://www.w3.org/1999/xlink"))
                                        #<!DOCTYPE gxl SYSTEM "http://www.gupro.de/GXL/gxl-1.0.1.dtd">
                                        #<gxl xmlns:xlink="http://www.w3.org/1999/xlink">
    out$addTag("gxl",close=FALSE)
    out$addTag("graph", attrs=c(id="graphNEL", edgemode=
                          as.character(edgemode(gnel))), close=FALSE)
    for (i in 1:length(nds))
      {
          out$addTag("node", attrs=c(id=nds[i]), close=FALSE)
          out$closeTag()
      }
    ued <- 0
    for (i in 1:length(eds))
      {
          if (length(eds[[i]])>0) for (j in 1:length(eds[[i]]))
            {
                ued <- ued + 1
                etag <- paste("e",ued,sep="")
                out$addTag("edge", attrs=c(id=etag,from=enms[i],
                                     to=eds[[i]][j]), close=FALSE)
                out$closeTag()
            }
      }
    out$closeTag() # graph
    out$closeTag() # gxl
    out
}


