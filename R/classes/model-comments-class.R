# definition of "modelCommentsClass"
# methods: "show"
#' S4 class for representing all the comments of a \code{rrisk} model
#'
#' @name modelCommentsClass-class
#' @aliases modelCommentsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model comments
#' @slot comments
#' @rdname modelCommentsClass-class
#' @exportClass modelCommentsClass
#' @examples
#'  \donttest{new("modelCommentsClass")
#'
#' new("modelCommentsClass",
#' comments=list(gsub(x="The structure of the model follows the scenario 
#' pathway. The grouping of items into parts could be re-arranged to 
#' be consistent with the structure of risk assessments according to 
#' Codex Alimentarius if necessary.","\n",replacement=""),
#' gsub(x="The sensitivity analysis indicates that among the uncertain model 
#' inputs, the probability of illness due to exposure to a single 
#' E.~coli cell ($r$) has a great impact on the outcome risk estimate.
#' The variability of the outcome is largely dependent on the number 
#' of viable bacteria in the dish at time of consumption ($n$)...",
#' "\n",replacement=""))) }

setClass(Class          = "modelCommentsClass",
         representation = representation(comments = "list"),
         prototype      = prototype(comments = list()))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelCommentsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelCommentsClass-method
#' @docType methods
#' @title Show method for 'modelCommentsClass'
#' @param object a \code{modelCommentsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelCommentsClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelCommentsClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Comments\n")
            cat("*************************************************************************\n")
            if (length(object@comments) == 0)
              cat("Mode comments list is empty\n\n")
            for(comments in object@comments)
              cat(putLineBreaks(comments), "\n\n")
          })