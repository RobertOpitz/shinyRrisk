# definition of "modelOutputClass"
# methods: "show"
#' S4 class for representing \code{rrisk} modelOutputClass
#'
#' @name modelOutputClass-class
#' @aliases modelOutputClass
#' @docType class
#' @title S4 class for representing 'rrisk' modelOutputClass
#' @slot fullout.1d  list-object
#' @slot relaxout.1d list-object
#' @slot fullout2d character-vector
#' @slot uncitems.2d character-vector
#' @slot OFname.2d character
#' @slot summaries character-vector
#' @slot runtime1d numeric
#' @slot runtime2d numeric
#' @slot OFcdfCI numeric
#' @rdname modelOutputClass-class
#' @exportClass modelOutputClass
#' @examples
#'  \donttest{new("modelOutputClass")}

setClass(Class          = "modelOutputClass",
         representation = representation(fullout.1d  = "list",
                                         relaxout.1d = "list",
                                         fullout.2d  = "ANY",
                                         uncitems.2d = "ANY",
                                         OFname.2d   = "character",
                                         summaries   = "ANY",
                                         #runtime1d   = "numericNULL",
                                         #runtime2d   = "numericNULL", 
                                         OFcdfCI     = "ANY"),
         prototype      = prototype(fullout.1d  = list(),
                                    relaxout.1d = list(),
                                    fullout.2d  = c(),
                                    uncitems.2d = c(),
                                    OFname.2d   = "",
                                    summaries   = c(),
                                    #runtime1d   = NULL,
                                    #runtime2d   = NULL, 
                                    OFcdfCI     = c()))  

#-------------------------------------------------------------------------------
#' Show method for \code{\linkS4class{modelOutputClass}}
#'
#' @rdname show-methods
#' @aliases show,modelOutputClass-method
#' @docType methods
#' @title Show method for 'modelOutputClass'
#' @param object a \code{modelOutputClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelOutputClass"))    }

setMethod(f          = "show", 
          signature  = signature(object = "modelOutputClass"), 
          definition = function(object) { 
            cat("*************************************************************************\n")
            cat("Model run results \n")
            cat("*************************************************************************\n")
            cat("\nEvaluated outcome function(s)\n")
            show(object@summaries)
            cat("\n")
            if (object@OFname.2d != "") 
              cat("2d similation has been evaluated for the outcome function",
                  object@OFname.2d, "\n")
            if (!is.null(object@runtime1d)) 
              cat("Run time of the 1d simulation", object@runtime1d, "sec.\n")
            if (!is.null(object@runtime2d)) 
              cat("Run time of the 2d simulation", object@runtime2d, "sec.\n")
            cat("\n")
          })