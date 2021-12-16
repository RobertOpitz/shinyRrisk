#' S4 class for representing basic descriptions of a \code{rrisk} model
#'
#' @name basicsClass-class
#' @aliases basicsClass
#' @docType class
#' @title S4 class for representing basic descriptions
#' @slot name
#' @slot explanation
#' @rdname basicsClass-class
#' @exportClass basicsClass
#' @examples
#' \donttest{new("basicsClass")
#'
#' new("basicsClass",
#' name="Background",
#' explanation=gsub(x="Escherichia (E.) coli O157:H7 represents a microbial 
#' hazard in food stuffs such as ground beef. It is known that good kitchen
#' hygiene and proper cooking reduces the risk of food-borne outbreaks
#' caused by this bacterium. A model for assessing the risk of illness
#' in children less than...","\n",replacement=""))}

setClass(Class          = "basicsClass",
         representation = representation(name        = "character",
                                         explanation = "character"),
         prototype      = prototype(name        = "",
                                    explanation = ""))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{basicsClass}}
#'
#' @rdname show-methods
#' @aliases show, basicsClass-method
#' @docType methods
#' @title Show method for 'basicsClass'
#' @param object a \code{basicsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("basicsClass"))}

setMethod(f          = "show",
          signature  = signature(object = "basicsClass"),
          definition = function(object) { 
            cat("Name:        ", object@name, "\n")
            cat("Explanation: ", putLineBreaks(object@explanation), "\n")
          }
)