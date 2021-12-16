# definition of "modelValidationClass"
# methods: "show"
#' S4 class for representing all the validities of a \code{rrisk} model
#'
#' @name modelValidationClass-class
#' @aliases modelValidationClass
#' @docType class
#' @title S4 class for representing 'rrisk' model validities
#' @slot validation
#' @rdname modelValidationClass-class
#' @exportClass modelValidationClass
#' @examples
#' \donttest{new("modelValidationClass")
#'
#' new("modelValidationClass",
#' validation=list(
#' new("validationClass",name="Model concept",
#' explanation=gsub(x="The model concept is a reasonable representation of 
#' the key processes that are to be considered to answer the risk question. 
#' The concept has been verified from a microbiological and modelling 
#' viewpoint.","\n",replacement="")),
#' new("validationClass",
#' name="Model implementation and verification",
#' explanation=gsub(x="The correctness of the translation of the model
#' concept into the mathematical model has been checked. 
#' The appropriateness of the model parameterisation---and in 
#' particular of the choice of distributions for stochastic model 
#' input---has been commented in the item descriptions. The unit of 
#' observation of each model input quantity was carefully considered 
#' when combining inputs by mathematical expressions...","\n",
#' replacement="")))) }

setClass(Class          = "modelValidationClass",
         representation = representation(validation = "list"),
         prototype      = prototype(validation = list()))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelValidationClass}}
#'
#' @rdname show-methods
#' @aliases show,modelValidationClass-method
#' @docType methods
#' @title Show method for 'modelValidationClass'
#' @param object a \code{modelValidationClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelValidationClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelValidationClass"),
          definition = function(object) { 
            cat("*************************************************************************\n")
            cat("Validation\n")
            cat("*************************************************************************\n")
            if (length(object@validation) == 0)
              cat("Model validation list is empty\n\n")
            for (validation in object@validation) { 
              show(validation)
              cat("\n")
            }
          })