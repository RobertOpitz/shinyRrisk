setMethod(f          = "toList", 
          signature  = signature(object = "modelItemsClass"), 
          definition = function(object) lapply(object@items, toList))
