# definition of "modelVersionClass"
# methods: "show"
#' A S4 class for representing basic information of a \code{rrisk} model version
#'
#' @name modelVersionClass-class
#' @aliases modelVersionClass
#' @docType class
#' @title S4 class for representing 'rrisk' model version
#' @slot status
#' @slot minorupdate
#' @slot majorupdate
#' @slot subtitle
#' @slot editedby
#' @rdname modelVersionClass-class
#' @exportClass modelVersionClass
#' @examples
#' \donttest{new("modelVersionClass")
#'
#' new("modelVersionClass",status="Draft",minorupdate=0,majorupdate=1,
#' subtitle="Space for subtitle",editedby="Matthias Greiner")}

setClass(Class          = "modelVersionClass",
         representation = representation(status      = "character",
                                         minorupdate = "numeric",
                                         majorupdate = "numeric",
                                         subtitle    = "character",
                                         editedby    = "character"),
         prototype      = prototype(status      = "Draft",
                                    minorupdate = 0,
                                    majorupdate = 0,
                                    subtitle    = "",
                                    editedby    = "")
)

#' Show method for \code{\linkS4class{modelVersionClass}}
#'
#' @rdname show-methods
#' @aliases show
#' @docType methods
#' @title Show method for 'modelVersionClass'
#' @param object a \code{modelVersionClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelVersionClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelVersionClass"),
          definition = function(object) { 
            cat("*************************************************************************\n")
            cat("Model version\n")
            cat("*************************************************************************\n")
            cat("Status:       ", object@status, "\n") 
            cat("Major update: ", object@majorupdate, "\n")
            cat("Minor update: ", object@minorupdate, "\n") 
            cat("Subtitle:     ", object@subtitle, "\n")
            cat("Edited by:    ", object@editedby, "\n\n")
          })