# definition of "modelSettingsClass"
# methods: "show"
#' S4 class for representing information of all the default settings of a \code{rrisk} model
#'
#' @name modelSettingsClass-class
#' @aliases modelSettingsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model settings
#' @slot wscores
#' @slot N
#' @slot Ntest
#' @slot N2d
#' @slot stress
#' @slot mycol
#' @slot nwlayout
#' @slot usenotapplicable
#' @slot coverheader
#' @slot trans
#' @slot sens
#' @slot sty
#' @slot deleteTeX
#' @slot abserror
#' @rdname modelSettingsClass-class
#' @exportClass modelSettingsClass
#' @examples
#' \donttest{show(new("modelSettingsClass"))}

setClass(Class          = "modelSettingsClass",
         representation = representation(wscores          = "character",
                                         N                = "numeric",
                                         Ntest            = "numeric",
                                         N2d              = "numeric",
                                         stress           = "numeric",
                                         mycol            = "character",
                                         nwlayout         = "character",
                                         usenotapplicable = "logical",
                                         coverheader      = "character",
                                         trans            = "character",
                                         sens             = "character",
                                         sty              = "character",
                                         deleteTeX        = "logical",
                                         abserror         = "numeric"),
         prototype      = prototype(wscores          = "equal",
                                    N                = 10000,
                                    Ntest            = 100,
                                    N2d              = 50,
                                    stress           = 90,
                                    mycol            = "lightblue",
                                    nwlayout         = "kamadakawai",
                                    usenotapplicable = TRUE,
                                    coverheader      = "model network",
                                    trans            = "rank",
                                    sens             = "correlation", 
                                    sty              = "Sweave",
                                    deleteTeX        = TRUE,
                                    abserror         = 0.0001))

#' Show method for \code{\linkS4class{modelSettingsClass}}
#'
#' @rdname show-methods
#' @aliases show, modelSettingsClass-method
#' @docType methods
#' @title Show method for 'modelSettingsClass'
#' @param object a \code{modelSettingsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelSettingsClass"))}

setMethod(f          = "show",
          signature  = signature(object="modelSettingsClass"),
          definition = function(object) { 
            cat("*************************************************************************\n")
            cat("Model settings\n")
            cat("*************************************************************************\n") 
            cat("Number of iterations (1st dimension):                    ", object@N, "\n")
            cat("Number of iterations (2nd dimension):                    ", object@N2d, "\n")
            cat("Number of test iterations:                               ", object@Ntest, "\n")
            cat("Stress test percentile:                                  ", object@stress, "\n")
            cat("Absolute error:                                          ", object@abserror, "\n")
            cat("Weights for qualitative scores:                          ", object@wscores, "\n")
            cat("Standard color for graphics:                             ", object@mycol, "\n")
            cat("Layout for model graph visualisation:                    ", object@nwlayout, "\n")
            cat("Use 'not applicable' by displaying model uncertainties : ", object@usenotapplicable, "\n")
            cat("Optional laTeX style files:                              ", object@sty, "\n")
            cat("Protect source file:                                     ", object@deleteTeX, "\n")
            cat("Cover picture for model report:                          ", object@coverheader,"\n")
            cat("Transformation for sensitivity analysis:                 ", object@trans, "\n")
            cat("Model for sensitivity analysis:                          ", object@sens, "\n\n")
          })