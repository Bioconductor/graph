.GXLformals <- function(where) {
#
# GXL support
#
## need methods on connections
     setClass("file", where=where)
     setClass("connection", where=where)
     setIs("file","connection", where=where)
## fromGXL returns the graphNEL object only, and it may
##  need to return more properties (7 mar 03)
     setGeneric("fromGXL",function(con)standardGeneric("fromGXL"),where=where)
     setMethod("fromGXL", "connection", function(con)
       {
       require(XML)
       xmlEventParse(paste(readLines(con),collapse=""),NELhandler(),asText=TRUE)$asGraphNEL()
       }, where=where)
## dumpGXL returns an R list with all? properties
     setGeneric("dumpGXL",function(con)standardGeneric("dumpGXL"),where=where)
     setMethod("dumpGXL", "connection", function(con)
       {
       require(XML)
       xmlEventParse(paste(readLines(con),collapse=""),NELhandler(),asText=TRUE)$dump()
       }, where=where)
## validate against the dtd
     setGeneric("validateGXL",function(con)standardGeneric("validateGXL"),where=where)
     setMethod("validateGXL", "connection", function(con)
       {
       require(XML)
# maybe need a try here, xmlTreeParse dumps the whole stream when it hits an error
       tmp <- xmlTreeParse(paste(readLines(con),collapse=""),asText=TRUE,
              validate=TRUE)
       }, where=where)
#
#  exporting
#
    setGeneric("toGXL", function(object)standardGeneric("toGXL"),where=where)
    setMethod("toGXL", "graphNEL", function(object) 
       gxlTreeNEL(object), where=where)
}
  
gxlTreeNEL <- function(gnel) {
 require(XML)
 nds <- nodes(gnel)
 eds <- lapply(edges(gnel),unique)
 enms <- names(eds)
 out <- xmlTree("gxl", dtd="http://www.gupro.de/GXL/gxl-1.0.1.dtd",
   namespaces=c(gxl="http://www.w3.org/1999/xlink"))
#<!DOCTYPE gxl SYSTEM "http://www.gupro.de/GXL/gxl-1.0.1.dtd">
#<gxl xmlns:xlink="http://www.w3.org/1999/xlink">
 out$addTag("gxl",close=FALSE)
 out$addTag("graph", attrs=c(id="graphNEL"), close=FALSE)
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


