# definition of "modelConclusionsClass"
# methods: "show"
#' S4 class for representing all the conclusions of a \code{rrisk} model
#'
#' @name modelConclusionsClass-class
#' @aliases modelConclusionsClass
#' @docType class
#' @title S4 class for representing all 'rrisk' model conclusions
#' @slot conclusions
#' @rdname modelConclusionsClass-class
#' @exportClass modelConclusionsClass
#' @examples
#'  \donttest{new("modelConclusionsClass")
#'
#' new("modelConclusionsClass",
#' conclusions=list(gsub(x="This illustrative model predicts that the population
#' average probability of illness due to E.coli O157:H7 in child below 
#' age of three years attributable to consumption of one serving prepared 
#' from ground beef is around...","\n",replacement=""),
#' gsub(x="The probabilistic modelling approach indicates that, taking into 
#' account variability and uncertainty, this probability could be much 
#' higher in rare cases...","\n",replacement=""))) }

setClass(Class          = "modelConclusionsClass",
         representation = representation(conclusions = "list"),
         prototype      = prototype(conclusions = list()))

#-------------------------------------------------------------------------------

#' Show method for \code{\linkS4class{modelConclusionsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelConclusionsClass-method
#' @docType methods
#' @title Show method for 'modelConclusionsClass'
#' @param object a \code{modelConclusionsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelConclusionsClass"))
#'
#' new("modelConclusionsClass",
#' conclusions=list(gsub(x="This illustrative model predicts that the population
#' average probability of illness due to E.coli O157:H7 in child below 
#' age of three years attributable to consumption of one serving prepared 
#' from ground beef is around...","\n",replacement=""),
#' gsub(x="The probabilistic modelling approach indicates that, taking into 
#' account variability and uncertainty, this probability could be much 
#' higher in rare cases...","\n",replacement="")))}

setMethod(f          = "show",
          signature  = signature(object="modelConclusionsClass"),
          definition = function(object){ 
            cat("*************************************************************************\n")
            cat("Conclusions\n")
            cat("*************************************************************************\n")
            if (length(object@conclusions) == 0)
              cat("Model conclusions list is empty\n\n")
            for(conclusion in object@conclusions)
              cat(putLineBreaks(conclusion), "\n\n")
          })