## Copyright Laurent Gautier, 2003

write.tlp <- function(graph, filename) {
  m <- edgeMatrix(graph)
  
  n <- numNodes(graph)

  con <- file(filename, open="w")

  ## nodes declaration
  writeLines("(nodes ", con, sep="")
  writeLines(as.character(seq(1, n, length=n)), con, sep=" ")
  writeLines(")\n", con, sep="")

  ## edges declaration
  for (i in seq(1, ncol(m), length=ncol(m))) {
    writeLines("(edge ", con, sep="")
    writeLines(as.character(c(i, m[1, i], m[2, i])), con, sep=" ")
    writeLines(")\n", con, sep="")
  }

  rm(m)
  
  ## nodes label declaration
  allnodes <- nodes(graph)
  
  writeLines("(property 0 string \"viewLabel\"\n", con, sep="")
  writeLines("(default \"\" \"\" )\n", con, sep="")

  for (i in seq(along=allnodes)) {
    writeLines(c("(node ", as.character(i), allnodes[i], ")\n"), con, sep=" ")
  }
  writeLines(")\n", con, sep="")
  
  close(con)
  
}
