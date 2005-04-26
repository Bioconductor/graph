.initGContents <- function() {
   if( !isGeneric("contents") && !exists("contents", mode="function") )
       setGeneric("contents", function(object, all.names)
                  standardGeneric("contents"))

    setMethod("contents", "hashtable",
              function(object, all.names) {
                  if(missing(all.names))
                      all.names = FALSE
                  as.list(object@hashtable, all.names)
              })
}
