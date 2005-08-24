.initGContents <- function() {
   if( !isGeneric("contents") && !exists("contents", mode="function") )

    setMethod("contents", "hashtable",
              function(object, all.names) {
                  if(missing(all.names))
                      all.names = FALSE
                  as.list(object@hashtable, all.names)
              })
}
