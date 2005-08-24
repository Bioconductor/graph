

setMethod("property",
          signature(x="propertyHolder", prop="character"),
          function (x, prop)
      {
          if (!(prop %in% names(x@property)))
              stop(paste("no property named", prop))
          x@property[[prop]]
      })


setReplaceMethod("property",
                 signature(x = "propertyHolder",
                           prop = "character"),
                 function (x, prop, value)
             {
                 if (!(prop %in% names(x@property)))
                     stop(paste("no property named", prop))
                 x@property[[prop]] <- value
                 x
             })


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

setMethod("asGraphProperty",
          signature(x="numeric", hasWeight="missing"),
          function (x, hasWeight)
      {
          x <- as.list(x)
          if (length(x) != 0 &&
              (length(names(x)) != length(x) ||
               any(is.na(names(x)))))
              stop("property list must have names for all elements")
          x
      })
