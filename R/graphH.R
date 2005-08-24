hashtable <- function() new.env(hash=TRUE)


setMethod("initialize",
          signature(.Object="graphH"),
          function(.Object,
                   graphID,
                   nodes=list(),
                   edges=hashtable(),
                   edgemode="undirected",
                   ...)
      {
          if (!is.list(nodes))
              stop("nodes must be an environment")
          if (is.list(edges)) {
              tmp <- hashtable()
              for (e in edges) {
                  tmp[[idstring(e)]] <- e
              }
              edges <- tmp
          }
          if (!is.environment(edges)) {
              stop("edges must be an environment or list")
          }
          nNodes <- length(nodes)
          if (nNodes == 0 && length(edges) != 0)
              stop("empty graph can not have edges")
          else {
              names(nodes) <-
                  lapply(nodes, function(n)
                     {
                         if (!is(n, "gNode"))
                             stop("all nodes must be of class gNode")
                         idstring(n)
                     })
              label2nodeID <- names(nodes)
              names(label2nodeID) <- lapply(nodes, label)
              if (length(edges) != 0) {
                  edgesNames <- ls(edges, all=TRUE)
                  ## This should be done in C
                  for (i in seq(along=nodes)) {
                      fromEdges(nodes[[i]]) <- list()
                      toEdges(nodes[[i]]) <- list()
                  }
                  for (eid in edgesNames) {
                      edge <- edges[[eid]]
                      if (idstring(edge) != eid)
                          stop("each should be assigned to its ID in `edges'")
                      if (edge@bNode == edge@eNode) {
                          n <- nodes[[as.character(edge@bNode)]]
                          n@fromEdges[[eid]] <- getuuid(eid)
                          n@toEdges[[eid]] <- getuuid(eid)
                      } else {
                          bn <- nodes[[as.character(edge@bNode)]]
                          en <- nodes[[as.character(edge@eNode)]]
                          bn@fromEdges[[eid]] <- getuuid(eid)
                          en@toEdges[[eid]] <- getuuid(eid)
                          if (!edge@directed) {
                              en@fromEdges[[eid]] <- getuuid(eid)
                              bn@toEdges[[eid]] <- getuuid(eid)
                          }
                      }
                      nodes[[as.character(edge@bNode)]] <- bn
                      nodes[[as.character(edge@eNode)]] <- en
                  }
                  if (nNodes != length(nodes))
                      stop("invalid edge information")
                  for (i in seq(along=nodes)) {
                      names(nodes[[i]]@fromEdges) <- NULL
                      names(nodes[[i]]@toEdges) <- NULL
                  }
              }
          }
          .Object@graphID <- getuuid()
          .Object@nodes <- nodes
          .Object@edges <- edges
          .Object@edgemode <- edgemode
          .Object@label2nodeID <- label2nodeID
          .Object
      })


setMethod("idstring",
          signature(x="gNode"),
          function(x) as.character(x@nodeID))

setMethod("idstring",
          signature(x="gEdge"),
          function(x) as.character(x@edgeID))

setMethod("idstring",
          signature(x="graphH"),
          function(x) as.character(x@graphID))

setMethod("nodes",
          signature(object="graphH"),
          function (object)
      {
          names(object@label2nodeID)
      })

setReplaceMethod("nodes",
                 signature(object = "graphH", value = "character"),
                 function (object, value)
             {
                 if (length(value) != length(object@nodes))
                     stop("need as many names as there are nodes")
                 if(any(duplicated(value)))
                     stop("node names must be unique")
                 n <- object@nodes
                 for (i in seq(along=object@nodes)) {
                     n[[i]]@label <- value[[i]]
                 }
                 object@nodes <- n
                 names(object@label2nodeID) <- value
                 object
             })


setMethod("getNodes",
          signature(x="graphH", which="missing"),
          function (x, which)
      {
          ans <- x@nodes
          names(ans) <- nodes(x)
          ans
      })

setMethod("getNodes",
          signature(x="graphH", which="character"),
          function (x, which)
      {
          ans <- x@nodes[x@label2nodeID[which]]
          names(ans) <- which
          ans
      })

setMethod("edges",
          signature(object="graphH", which="missing"),
          function (object, which)
      {
          nm <- nodes(object)
          names(nm) <- object@label2nodeID
          edgeEnv <- object@edges
          ans <- lapply(object@nodes,
                        function(node)
                    {
                        tmp <- nm[unlist(lapply(fromEdges(node),
                                                function(eid)
                                            {
                                                edge <- edgeEnv[[as.character(eid)]]
                                                if (edge@directed ||
                                                    edge@eNode != nodeID(node))
                                                    as.character(edge@eNode)
                                                else as.character(edge@bNode)
                                            }))]
                        names(tmp) <- NULL
                        tmp
                    })
          names(ans) <- nm
          ans
      })

setMethod("edges",
          signature(object="graphH", which="character"),
          function (object, which)
      {
          nm <- nodes(object)
          names(nm) <- object@label2nodeID
          edgeEnv <- object@edges
          ans <- lapply(object@nodes[object@label2nodeID[which]],
                        function(node)
                    {
                        tmp <- nm[unlist(lapply(fromEdges(node),
                                                function(eid)
                                                as.character(edgeEnv[[as.character(eid)]]@eNode)))]
                        names(tmp) <- NULL
                        tmp
                    })
          names(ans) <- which
          ans
      })

setMethod("degree",
          signature(object="graphH", Nodes="missing"),
          function (object, Nodes)
      {
          deg <- unlist(lapply(object@nodes,
                               function(x) length(x@fromEdges)))
          names(deg) <- nodes(object)
          if( object@edgemode == "undirected" )
              return(deg)
          else if( object@edgemode == "directed" ) {
              inDegree <- unlist(lapply(object@nodes,
                                        function(x) length(x@fromEdges)))
              names(inDegree) <- names(deg)
              return(list(inDegree=inDegree, outDegree=deg))
          }
          stop(paste("edgemode", object@edgemode, "is not valid"))
      })

setMethod("degree",
          signature(object="graphH", Nodes="character"),
          function (object, Nodes)
      {
          deg <- unlist(lapply(object@nodes[object@label2nodeID[Nodes]],
                               function(x) length(x@fromEdges)))
          names(deg) <- nodes(object)
          if( object@edgemode == "undirected" )
              return(deg)
          else if( object@edgemode == "directed" ) {
              inDegree <-
                  unlist(lapply(object@nodes[object@label2nodeID[Nodes]],
                                function(x) length(x@fromEdges)))
              names(inDegree) <- names(deg)
              return(list(inDegree=inDegree, outDegree=deg))
          }
          stop(paste("edgemode", object@edgemode, "is not valid"))
      })
