# definition of "modelClass"
# methods: "show"
#' S4 class for representing \code{rrisk} models
#'
#' @name modelClass-class
#' @aliases modelClass
#' @docType class
#' @title S4 class for representing 'rrisk' models
#' @slot name
#' @slot modeltype
#' @slot version
#' @slot authors
#' @slot settings
#' @slot scoring
#' @slot basics
#' @slot references
#' @slot uncertainties
#' @slot parts
#' @slot graphs
#' @slot glossary
#' @slot abbreviations
#' @slot items
#' @slot validation
#' @slot comments
#' @slot conclusions
#' @slot output
#' @slot archive
#' @rdname modelClass-class
#' @exportClass modelClass
#' @examples
#'  \donttest{new("modelClass")}

setClass(Class          = "modelClass",
         representation = representation(name          = "modelNameClass",
                                         modeltype     = "character",
                                         version       = "modelVersionClass",
                                         authors       = "modelAuthorsClass",
                                         settings      = "modelSettingsClass",
                                         scoring       = "modelScoringClass",
                                         basics        = "modelBasicsClass",
                                         references    = "modelReferencesClass",
                                         uncertainties = "modelUncertaintiesClass",
                                         parts         = "modelPartsClass",
                                         graphs        = "modelGraphsClass",
                                         glossary      = "modelGlossaryClass",
                                         abbreviations = "modelAbbreviationsClass",
                                         items         = "modelItemsClass",
                                         validation    = "modelValidationClass",
                                         comments      = "modelCommentsClass",
                                         conclusions   = "modelConclusionsClass",
                                         output        = "modelOutputClass",
                                         archive       = "ANY"),
         prototype      = prototype(name          = new("modelNameClass"),
                                    modeltype     = "user defined",
                                    version       = new("modelVersionClass"),
                                    authors       = new("modelAuthorsClass"),
                                    settings      = new("modelSettingsClass"),
                                    scoring       = new("modelScoringClass"),
                                    basics        = new("modelBasicsClass"),
                                    references    = new("modelReferencesClass"),
                                    uncertainties = new("modelUncertaintiesClass"),
                                    parts         = new("modelPartsClass"),
                                    graphs        = new("modelGraphsClass"),
                                    glossary      = new("modelGlossaryClass"),
                                    abbreviations = new("modelAbbreviationsClass"),
                                    items         = new("modelItemsClass"),
                                    validation    = new("modelValidationClass"),
                                    comments      = new("modelCommentsClass"),
                                    conclusions   = new("modelConclusionsClass"),
                                    output        = new("modelOutputClass"),
                                    archive       = NULL))

#' Show method for \code{\linkS4class{modelClass}}
#'
#' @rdname show-methods
#' @aliases show,modelClass-method
#' @docType methods
#' @title Show method for 'modelClass'
#' @param object a \code{modelClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("modelClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelClass"),
          definition = function(object) { 
            show(object@name)
            cat("*************************************************************************\n")
            cat("Model type\n")
            cat("*************************************************************************\n")
            cat(object@modeltype,"\n\n")
            show(object@version)
            show(object@authors)
            show(object@settings)
            show(object@scoring)
            show(object@basics)
            show(object@references)
            show(object@uncertainties)
            show(object@parts)
            show(object@items)
            show(object@graphs)
            show(object@glossary)
            show(object@abbreviations)
            show(object@output)
            show(object@validation)
            show(object@comments)
            show(object@conclusions)
            #-------------------------------------------------------------------
            cat("*************************************************************************\n")
            cat("Archive\n")
            cat("*************************************************************************\n")
            if (!is.null(object@archive))
              cat("There is one old model version in the archive...\n\n")
            else 
              cat("The archive is empty...\n\n")
          })