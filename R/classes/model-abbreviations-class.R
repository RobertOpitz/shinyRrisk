# definition of "modelAbbreviationsClass"
# methods: "show"
#' S4 class for representaing all the abbreviations uesed in a \code{rrisk} model
#'
#' @name modelAbbreviationsClass-class
#' @aliases modelAbbreviationsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model abbreviations list
#' @slot abbreviations
#' @rdname modelAbbreviationsClass-class
#' @exportClass modelAbbreviationsClass
#' @examples
#' \donttest{new("modelAbbreviationsClass")
#'
#' new("modelAbbreviationsClass",
#' abbreviations=list(
#' new("glossaryClass",name="cfu", explanation="colony forming units"),
#' new("glossaryClass",name="cfu", explanation="colony forming units")))}

setClass(Class          = "modelAbbreviationsClass",
         representation = representation(abbreviations = "list"),
         prototype      = prototype(abbreviations = list()))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelAbbreviationsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelAbbreviationsClass-method
#' @docType methods
#' @title Show method for 'modelAbbreviationsClass'
#' @param object a \code{modelAbbreviationsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelAbbreviationsClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelAbbreviationsClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Abbreviations\n")
            cat("*************************************************************************\n")
            if (length(object@abbreviations) == 0)
              cat("Model abbreviations list is empty\n\n")
            for (abbrev in object@abbreviations) { 
              show(abbrev)
              cat("\n")
            }
          })