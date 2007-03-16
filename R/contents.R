.initGContents <- function() {
    setMethod("contents", "hashtable",
              function(object, all.names) {
                  if(missing(all.names))
                      all.names = FALSE
                  as.list(object@hashtable, all.names)
              })
}
