.initGContents <- function(where) {
   if( !isGeneric("contents") && !exists("contents", mode="function") )
       setGeneric("contents", function(object, all.names)
                  standardGeneric("contents"), where=where)

    setMethod("contents", "hashtable",
              function(object, all.names) {
                  if(missing(all.names))
                      all.names = FALSE
                  as.list(object@hashtable, all.names)
              }, where=where)
}
