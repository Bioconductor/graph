
#setClass("compoundGraph",
# representation(grList="list",
#  between="list"))

#setGeneric("grList",function(object)standardGeneric("grList"))
#setMethod("grList","compoundGraph", function(object)object@grList)
#
#setGeneric("between",function(object)standardGeneric("between"))
#setMethod("between", "compoundGraph", function(object)object@between)

setGeneric("toDotR", function(G, outDotFile, renderList, optList)standardGeneric("toDotR"))
setMethod("toDotR", c("graphNEL", "character", "list", "list"),
function (G, outDotFile, renderList, optList=.standardToDotOptions) 
{

buildEdge <- function(fromTok, toTok, opts, labField=NULL) {
 protq <- function(x) paste("\"",x,"\"",sep="")
 core <- paste(protq(fromTok),"->",protq(toTok),";\n",sep=" ")
 UDB <- opts$useDirBack
 ELF <- opts$edgeLabelField
 if (length(UDB) == 0) UDB <- FALSE
 if (length(ELF) == 0 || nchar(ELF) == 0 || is.null(labField)) ELF <- FALSE
   else ELF <- TRUE
 if (!UDB & !ELF) return(core)
 if (UDB & !ELF) return( paste("edge [dir=back]", core, sep=" "))
 if (!UDB & ELF) return( paste("edge [label=", labField,"] ", 
          core, sep=""))
 if (UDB & ELF) return( paste("edge [dir=back label=", 
          labField,"] ", core, sep=""))
 stop("logic error")
 }

 
# to get bottom to top orientation (B points up to A), use [dir=back] A->B
#
    if (is.null(renderList$start)) renderList$start <- "digraph G"
    out <- paste(renderList[["start"]], " {\n", sep="")
    ned <- length(E <- edgeL(G))
    enms <- names(E)
    nds <- nodes(G)
    ac <- as.character
    if (!is.null(pn <- renderList[["prenodes"]]))
       out <- paste(out, pn, "\n")
# need quote marks
    protq <- function(x) paste("\"",x,"\"",sep="")
# this takes care of isolated nodes if present
    for (j in nds) out <- paste( out, protq(j), ";\n" )
# deal with an edge statement
    if (!is.null(pe <- renderList[["preedges"]]))
       out <- paste(out, pe, "\n")
 if (ned > 0)
    for (i in 1:ned) 
      {
      if ((L <- length(E[[i]]$edges)) > 0)
        for (j in 1:L)
        {
        out <- paste(out, buildEdge( from=nds[ E[[i]]$edges[j] ],
         to=enms[i], optList, E[[i]][[ optList$edgeLabelField ]] ), sep=" ")
        }
      }
    out <- paste(out, "}\n", sep = "", collapse = "")
    if (outDotFile != ".AS.STRING")
      {
      cat(out, file = outDotFile)
      paste("dot file written to", 
           outDotFile, " use 'dot -Tps [.dot] [.ps] to render.\n")
      invisible(0)
      }
    else out
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


#function (G, outDotFile, start, optList) 
#{
## fakes structure to get bottom to top orientation
#    startStr <- start[[1]]
#    ext <- start[[2]]
#    out <- paste(startStr, " {\n", ext, sep="")
#    ned <- length(E <- edgeL(G))
#    enms <- names(E)
#    nds <- nodes(G)
## this takes care of isolated nodes if present
#    for (j in nds) out <- paste( out, j, ";\n" )
#    for (i in 1:ned) 
#    ac <- as.character
#    for (i in 1:ned) 
#      {
#      if ((L <- length(E[[i]]$edges)) > 0)
#        for (j in 1:L)
#        {
#        out <- paste(out, " edge [dir=back] \"", nds[ E[[i]]$edges[j] ],
#        "\"->\"", enms[i], "\";\n", sep = "", collapse = "")
#        }
#      }
#    out <- paste(out, "}", sep = "", collapse = "")
#    #cat(out, file = outDotFile)
#    #paste("dot file written to", outDotFile, " use 'dot -Tps [.dot] [.ps] to render")
#    out
#})

#setMethod("toDotR", c("compoundGraph", "character", "list", "missing"),
#function(G, outDotFile, renderList, optList) toDotR(G, outDotFile, renderList,
#  .standardToDotOptions))

#setMethod("toDotR", c("compoundGraph", "missing", "list", "missing"),
#function(G, outDotFile, renderList, optList) toDotR(G, ".AS.STRING", renderList,
#  .standardToDotOptions))

#setMethod("toDotR", c("compoundGraph", "character", "list", "list"),
#function(G, outDotFile, renderList, optList) {
## here renderList is a compound renderlist, one renderList per
## element of compound graph.  this list of renderlists
## must have something like "subgraph cluster_" as start
#   ng <- length( GL <- grList(G) )
#   rendL <- renderList #compRenderList(G)
#   out <- "digraph G {\n"
#   start <- rendL[[1]]$start
#   if (is.null(start)) start <- "subgraph cluster_"
#   sgn <- paste(start,1:ng,sep="")
#   for (i in 1:ng)
#     {
#     if (length(rendL) == 0) ext <- NULL
#       else ext <- rendL[[i]]
#     out <- paste(out, toDotR(GL[[i]],,
#              list( start=paste(sgn[i],"\n",sep=""),
#                    prenodes=ext$prenodes, preedges=ext$preedges), optList)
#                 ,sep="")
#     }
#
# done with subgraphs, now deal with between stuff
#
#   if (!is.null(rendL[[i+1]]))
#      out <- paste(out, rendL[[i+1]]$preedges)
#   if ((L <- length(BG <- between(G))) > 0)
#        for (j in 1:L)
#        {
#        out <- paste(out, " edge [dir=back] \"", BG[[ j ]][2],
#        "\"->\"", BG[[ j ]][1], "\";\n", sep = "", collapse = "")
#        }
#   out <- paste(out, "}\n", sep = "", collapse = "")
#   cat(out, file = outDotFile)
#   cat("dot file written to", outDotFile, " use 'dot -Tps [.dot] [.ps] to render\n")
#})

#setGeneric("adjMat",function(cg,ordvec)standardGeneric("adjMat"))
#setMethod("adjMat", c("compoundGraph", "ANY"), function(cg, ordvec) {
# if (length(ordvec)>2) stop("must specify indices of source and sink in ordvec, length(ordvec)==2")
# arows <- nodes(grList(cg)[[ordvec[1]]])
# acols <- nodes(grList(cg)[[ordvec[2]]])
# adjm <- matrix(0, nr=length(arows), nc=length(acols))
# dimnames(adjm) <- list(arows,acols)
# for (arc in between(cg))
#  adjm[arc[1], arc[2]] <- 1
# adjm
#})
##}
 
.standardToDotOptions <- list( useDirBack=TRUE )
