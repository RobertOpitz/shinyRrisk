# definition of "uncertClass"
# methods: "show", "toList"
#' S4 class for representing descriptions of an uncertainty of a \code{rrisk} model
#'
#' @name uncertClass-class
#' @aliases uncertClass
#' @docType class
#' @title S4 class for representing model uncertainty item
#' @slot namemain main category of uncertainty
#' @slot namesub sub category of uncertainty
#' @slot explanation
#' @slot scores
#' @rdname uncertClass-class
#' @exportClass uncertClass
#' @examples
#' \donttest{new("uncertClass")}

setClass(Class          = "uncertClass",
         representation = representation(namemain    = "character",
                                         namesub     = "character",
                                         explanation = "character",
                                         scores      = "numericNULL"),
         prototype      = prototype(namemain    = "",
                                    namesub     = "",
                                    explanation = "",
                                    scores      = c()))

#' Show method for \code{\linkS4class{uncertClass}}
#'
#' @rdname show-methods
#' @aliases show,uncertClass-method
#' @docType methods
#' @title Show method for 'uncertClass'
#' @param object a \code{uncertClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("uncertClass"))}

setMethod(f          = "show",
          signature  = signature(object = "uncertClass"),
          definition = function(object) { 
            cat("Main category: ", object@namemain, "\n")
            cat("Sub category: ", object@namesub, "\n")
            cat("Explanation: ", putLineBreaks(object@explanation), "\n")
            cat("Scores:", object@scores, "\n")
          })

#' toList method for \code{\linkS4class{uncertClass}}
#'
#' @rdname toList-methods
#' @aliases toList, uncertClass-method
#' @docType methods
#' @title toList method for 'uncertClass'
#' @param object a \code{uncertClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#' \donttest{toList(new("uncertClass"))}

setMethod(f          = "toList", 
          signature  = signature(object = "uncertClass"), 
          definition = function(object) {
            list(namemain    = object@namemain, 
                 namesub     = object@namesub, 
                 explanation = object@explanation)})
