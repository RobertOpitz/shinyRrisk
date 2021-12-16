# definition of "modelGlossaryClass"
# methods: "show"
#' S4 class for representing all the glossaries of a \code{rrisk} model
#'
#' @name modelGlossaryClass-class
#' @aliases modelGlossaryClass
#' @docType class
#' @title S4 class for representing 'rrisk' model glossary list
#' @slot glossary
#' @rdname modelGlossaryClass-class
#' @exportClass modelGlossaryClass
#' @examples
#' \donttest{new("modelGlossaryClass")}

setClass(Class          = "modelGlossaryClass",
         representation = representation(glossary = "list"),
         prototype      = prototype(glossary = list()))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelGlossaryClass}}
#'
#' @rdname show-methods
#' @aliases show,modelGlossaryClass-method
#' @docType methods
#' @title Show method for 'modelGlossaryClass'
#' @param object a \code{modelGlossaryClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelGlossaryClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelGlossaryClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Glossary\n")
            cat("*************************************************************************\n")
            if (length(object@glossary) == 0)
              cat("Model glossary list is empty\n\n")
            for (glossary in object@glossary) { 
              show(glossary)
              cat("\n")
            }
          })