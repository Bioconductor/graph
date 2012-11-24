
#setClass("compoundGraph",
# representation(grList="list",
#  between="list"))


#setMethod("grList","compoundGraph", function(object)object@grList)
#

#setMethod("between", "compoundGraph", function(object)object@between)


setMethod("toDotR", c("graphNEL", "character", "list", "list"),
    function (G, outDotFile, renderList, optList=.standardToDotOptions) 
{
    buildEdge <- function(fromTok, toTok, opts, labField=NULL) {
        protq <- function(x) paste0("\"",x,"\"")
        core <- paste(protq(fromTok),"->",protq(toTok),";\n",sep=" ")
        UDB <- opts$useDirBack
        ELF <- opts$edgeLabelField
        if (length(UDB) == 0) UDB <- FALSE
        if (length(ELF) == 0 || nchar(ELF) == 0 || is.null(labField))
            ELF <- FALSE
        else ELF <- TRUE
        if (!UDB & !ELF)
            return(core)
        if (UDB & !ELF)
            return(paste("edge [dir=back]", core, sep=" "))
        if (!UDB & ELF)
            return( paste0("edge [label=", labField,"] ", core))
        if (UDB & ELF)
            return(paste0("edge [dir=back label=", labField,"] ", core))
        stop("logic error")
    }
    
    ## to get bottom to top orientation (B points up to A), use
    ## [dir=back] A->B
    if (is.null(renderList$start)) renderList$start <- "digraph G"
    out <- paste0(renderList[["start"]], " {\n")
    ned <- length(E <- edgeL(G))
    enms <- names(E)
    nds <- nodes(G)
    ac <- as.character
    if (!is.null(pn <- renderList[["prenodes"]]))
        out <- paste(out, pn, "\n")
    ## need quote marks
    protq <- function(x) paste0("\"", x, "\"")
    ## this takes care of isolated nodes if present
    for (j in nds) out <- paste( out, protq(j), ";\n" )
    ## deal with an edge statement
    if (!is.null(pe <- renderList[["preedges"]]))
        out <- paste(out, pe, "\n")
    if (ned > 0)
        for (i in seq_len(ned)) {
            if ((L <- length(E[[i]]$edges)) > 0)
                for (j in seq_len(L)) {
                    builtEdge <- buildEdge(from=nds[ E[[i]]$edges[j] ],
                                           to=enms[i], optList,
                                           E[[i]][[ optList$edgeLabelField ]])
                    out <- paste(out, builtEdge, sep=" ")
                }
        }
    out <- paste(out, "}\n", sep = "", collapse = "")
    if (outDotFile != ".AS.STRING") {
        cat(out, file = outDotFile)
        paste("dot file written to", 
              sQuote(outDotFile), " use 'dot -Tps [.dot] [.ps] to render.\n")
        invisible(0)
    } else out
})

setMethod("toDotR", c("graphNEL", "character", "missing", "missing"),
 function(G, outDotFile, renderList, optList) toDotR(G, outDotFile, list(start="digraph G"), .standardToDotOptions))
# where=where)

setMethod("toDotR", c("graphNEL", "missing", "missing", "missing"),
 function(G, outDotFile, renderList, optList) toDotR(G, , list(start="digraph G"), .standardToDotOptions))
 #where=where)

setMethod("toDotR", c("graphNEL", "missing", "character", "missing"),
 function (G, outDotFile, renderList, optList) toDotR(G, ".AS.STRING", list(start=renderList," "), .standardToDotOptions))
# where=where)
 
setMethod("toDotR", c("graphNEL", "missing", "list", "list"),
 function(G, outDotFile, renderList, optList) toDotR(G, ".AS.STRING" , renderList, optList))
# where=where)

setMethod("toDotR", c("graphNEL", "missing", "list", "missing"),
 function(G, outDotFile, renderList, optList) toDotR(G, ".AS.STRING" , renderList, .standardToDotOptions))
# where=where)

setMethod("toDotR", c("graphNEL", "missing", "missing", "list"),
 function(G, outDotFile, renderList, optList) toDotR(G, ".AS.STRING" , list(start="digraph G"), optList))
# where=where)

setMethod("toDotR", c("graphNEL", "character", "missing", "list"),
 function(G, outDotFile, renderList, optList) toDotR(G, outDotFile , list(start="digraph G"), optList))
# where=where)

 
.standardToDotOptions <- list( useDirBack=TRUE )
