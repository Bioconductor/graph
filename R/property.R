setClass("propertyHolder",
         representation(property="list"))

if (!isGeneric("property"))
    setGeneric("property",
               function(x, prop)
               standardGeneric("property"))

setMethod("property",
          signature(x="propertyHolder", prop="character"),
          function (x, prop) x@property[[prop]])

if (!isGeneric("property<-"))
    setGeneric("property<-",
               function(x, prop, value)
               standardGeneric("property<-"))

setReplaceMethod("property",
                 signature(x = "propertyHolder"),
                 function (x, prop, value)
             {
                 x@property[[prop]]
             })

if (!isGeneric("asGraphProperty"))
    setGeneric("asGraphProperty",
               function(x, hasWeight)
               standardGeneric("asGraphProperty"))

setMethod("asGraphProperty",
          signature(x="list", hasWeight="missing"),
          function (x, hasWeight)
      {
          if (length(x) != 0 &&
              (length(names(x)) != length(x) ||
               any(is.na(names(x)))))
              stop("property list must have names for all elements")
          x
      })

setMethod("asGraphProperty",
          signature(x="list", hasWeight="logical"),
          function (x, hasWeight)
      {
          if (hasWeight && !is.numeric(x$weight))
              stop("property list must have a weight element")
          if (length(x) != 0 &&
              (length(names(x)) != length(x) ||
               any(is.na(names(x)))))
              stop("property list must have names for all elements")
          x
      })

setMethod("asGraphProperty",
          signature(x="numeric", hasWeight="logical"),
          function (x, hasWeight)
      {
          x <- as.list(x)
          if (length(x) == 1 && is.null(names(x)) && hasWeight)
              names(x) <- "weight"
          else {
              if (hasWeight && !("weight" %in% names(x)))
                  stop("property list must have a weight element")
              if (length(x) != 0 &&
                  (length(names(x)) != length(x) ||
                   any(is.na(names(x)))))
                  stop("property list must have names for all elements")
          }
          x
      })
