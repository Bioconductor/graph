#
# GXL support
#

## fromGXL returns the graphNEL object only, and it may
##  need to return more properties (7 mar 03)

setMethod("fromGXL", signature(con="connection"),
          function(con) {
              contents <- paste(readLines(con), collapse="")
              xmlEventParse <- getExportedValue("XML", "xmlEventParse")
              xmlEventParse(contents, graph_handler(),
                            asText=TRUE,
                            saxVersion=2)$asGraphNEL()
          })


## dumpGXL returns an R list with all? properties

     setMethod("dumpGXL", "connection", function(con)
       {
       xmlEventParse <- getExportedValue("XML", "xmlEventParse")
       xmlEventParse(paste(readLines(con), collapse=""),
                     NELhandler(),asText=TRUE)$dump()
       })
## validate against the dtd

     setMethod("validateGXL", "connection", function(con)
       {
       xmlTreeParse <- getExportedValue("XML", "xmlTreeParse")
# maybe need a try here, xmlTreeParse dumps the whole stream when it hits an error
       tmp <- xmlTreeParse(paste(readLines(con), collapse=""),
              asText=TRUE, validate=TRUE)
       })
#
#  exporting
#

    setMethod("toGXL", signature(object="graphNEL"),
              function(object, graph.name) {
                  if (missing(graph.name)) {
                      graph.name <- class(object)[1]
                  }
                  gxlTreeNEL(object, graph.name)
              })


gxlTreeNEL <- function(gnel, graph.name) {
    qrequire("XML")
    GXL_NAMESPACE <- c(gxl="http://www.gupro.de/GXL/gxl-1.1.dtd")
    out <- XML::xmlOutputDOM("gxl", nsURI=GXL_NAMESPACE, nameSpace="gxl")
    ## NOTE: We could specify dtd="http://www.gupro.de/GXL/gxl-1.0.1.dtd",
    ##       but this might mean that net access is required to write
    ##       GXL which seems quite unacceptable.
    nodeAttrs <- names(nodeDataDefaults(gnel))
    edgeAttrs <- names(edgeDataDefaults(gnel))

    writeAttr <- function(attrName, val) {
        ## skip NA and NULL
        if (is.null(val) || is.na(val))
          return(NULL)
        ## at present, can only handle length 1
        if (length(val) > 1) {
            warning("GXL conversion only handles attributes ",
                    "with length 1.  Will try to represent ",
                    "object of length ", length(val), " as a",
                    "string.")
            val <- paste(val, collapse=", ")
        }
        atag <- switch(typeof(val),
                       integer="int",
                       character="string",
                       double="float",
                       {
                           warning("I don't know how to convert ",
                                   "a ", typeof(val), " to GXL. ",
                                   " Skipping.")
                           NULL
                       })
        if (is.null(atag))
          return(NULL)
        out$addTag("attr", attrs=c(name=attrName), close=FALSE)
        out$addTag(atag, as.character(val))
        out$closeTag()
    }
    
    writeNode <- function(n) {
        ## Helper function to write a graphNEL node to XML
        out$addTag("node", attrs=c(id=n), close=FALSE)
        for (nodeAttr in nodeAttrs) {
            val <- nodeData(gnel, n, attr=nodeAttr)[[1]]
            writeAttr(nodeAttr, val)
        }
        out$closeTag() ## node
    }

    edgeCount <- 1
    writeEdge <- function(from, to) {
        ## Helper function to write a graphNEL node to XML
        edgeId <- edgeCount
        edgeCount <<- edgeCount + 1
        out$addTag("edge", attrs=c(id=edgeId, from=from, to=to),
                   close=FALSE)
        for (edgeAttr in edgeAttrs) {
            val <- edgeData(gnel, from, to, attr=edgeAttr)[[1]]
            writeAttr(edgeAttr, val)
        }
        out$closeTag() ## node
    }
    
    nds <- nodes(gnel)
    if (!isDirected(gnel)) {
        ## remove recipricol edges
        eds <- lapply(gnel@edgeL, function(x) x$edges)
        eds <- mapply(function(x, y) x[x < y], eds, seq(length=length(eds)))
        names(eds) <- nodes(gnel)
        eds <- lapply(eds, function(x) {
            if (length(x) > 0)
              nds[x]
            else
              character(0)
        })
    } else {
        eds <- edges(gnel)
    }
    enms <- names(eds)
    out$addTag("graph", attrs=c(id=graph.name, edgemode=edgemode(gnel)),
               close=FALSE)
    for (n in nds) {
        writeNode(n)
    }
    for (from in enms) {
        for (to in eds[[from]]) {
            writeEdge(from=from, to=to)
        }
    }
    out$closeTag() # graph
    out
}


