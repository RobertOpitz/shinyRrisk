# definition of "modelScoringClass"
# methods: "show", "toList"
#' S4 class for representing scoring systems of \code{rrisk} models
#'
#' @name modelScoringClass-class
#' @aliases modelScoringClass
#' @docType class
#' @title S4 class for representing 'rrisk' model scoring system
#' @slot name
#' @slot tableheader
#' @slot explanatory
#' @slot values
#' @slot vcolors
#' @slot vmeanings
#' @slot systemtype
#' @slot scoring
#' @rdname modelScoringClass-class
#' @exportClass modelScoringClass
#' @examples
#'  \donttest{new("modelScoringClass")}

setClass(Class          = "modelScoringClass",
         representation = representation(name        = "character",
                                         tableheader = "character",
                                         explanatory = "character",
                                         values      = "numeric",
                                         vcolors     = "numeric",
                                         vmeanings   = "numeric",
                                         systemtype  = "character",
                                         scoring     = "list"),
         prototype      = prototype(name        = "empty scoring system",
                                    tableheader = "",
                                    explanatory = "",
                                    values      = c(),
                                    vcolors     = c(),
                                    vmeanings   = c(),
                                    systemtype  = "user defined",
                                    scoring     = list()))

#' Show method for \code{\linkS4class{modelScoringClass}}
#'
#' @rdname show-methods
#' @aliases show,modelScoringClass-method
#' @docType methods
#' @title Show method for 'modelScoringClass'
#' @param object a \code{modelScoringClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelScoringClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelScoringClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Scoring system\n")
            cat("*************************************************************************\n")
            cat("Name: ",         object@name, "\n")
            cat("System type: ",  object@systemtype, "\n")
            cat("Table header: ", putLineBreaks(object@tableheader), "\n")
            cat("Explanatory: ",  putLineBreaks(object@explanatory), "\n")
            cat("Values: ",       object@values, "\n")
            #-------------------------------------------------------------------
            vcolors <- mapply(function(object_vcolors, 
                                       vcolor_names) {
                                paste0(object_vcolors, "=", vcolor_names)
                              }, object@vcolors, names(object@vcolors))
            cat("Colors: ",vcolors,"\n")
            #-------------------------------------------------------------------
            vmeanings <- mapply(function(object_vmeanings, vmeanings_names) {
                                  paste0(object_vmeanings, "=", vmeanings_names)
                                }, object@vmeanings, names(object@vmeanings))
            cat("Meanings: ",vmeanings,"\n\n")
            #-------------------------------------------------------------------
            for(scoring in object@scoring){ 
              show(scoring)
              cat("\n")
            }
          })

#' toList method for \code{\linkS4class{modelScoringClass}}
#'
#' @rdname toList-methods
#' @aliases toList, modelScoringClass-method
#' @docType methods
#' @title toList method for 'modelScoringClass'
#' @param object a \code{modelScoringClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#'  \donttest{toList(new("modelScoringClass"))}

setMethod(f          = "toList", 
          signature  = signature(object = "modelScoringClass"), 
          definition = function(object) lapply(object@scoring, toList))
