# definition of "modelClass"
# methods: "show"
#' S4 class for representing basic descriptions of a \code{rrisk} model
#'
#' @name modelBasicsClass-class
#' @aliases modelBasicsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model basic descriptions
#' @slot basics
#' @rdname modelBasicsClass-class
#' @exportClass modelBasicsClass
#' @examples
#' \donttest{new("modelBasicsClass")}

setClass(Class          = "modelBasicsClass",
         representation = representation(basics = "list"),
         prototype      = prototype(basics = list(new(Class       = "basicsClass",
                                                      name        = "mainDoc",
                                                      explanation = "Please enter here a reference to the main document (e.g., project report) in which the model is described..."),
                                                  new(Class       = "basicsClass",
                                                      name        = "Background",
                                                      explanation = "Please write here about the background of the model..."),
                                                  new(Class       = "basicsClass",
                                                      name        = "Objectives",
                                                      explanation = "Please write here about the purpose and the objectives of the model..."),
                                                  new(Class       = "basicsClass",
                                                      name        = "Scope",
                                                      explanation = "Please write here about the scope and limitations of the scope..."),
                                                  new(Class       = "basicsClass",
                                                      name        = "Description",
                                                      explanation = "Please write here the model itself (type of model, structure (division into parts),the processes modeled and which endpoint(s) will be simulated by the model)...")))
)

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelBasicsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelBasicsClass-method
#' @docType methods
#' @title Show method for 'modelBasicsClass'
#' @param object a \code{modelBasicsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelBasicsClass"))}

setMethod(f="show",
          signature=signature(object="modelBasicsClass"),
          definition=function(object) { 
            cat("*************************************************************************\n")
            cat("Basic descriptions\n")
            cat("*************************************************************************\n")
            if (length(object@basics) == 0)
              cat("Model basics list is empty \n\n")
            for (basics in seq_along(object@basics)) { 
              show(basics)
              cat("\n")
            }
          }
)
