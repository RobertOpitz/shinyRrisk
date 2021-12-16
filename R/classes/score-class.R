# definition of "scoreClass"
# methods: "show", "toList"
#' S4 class for representing information of a scoring system of a \code{rrisk} model
#'
#' @name scoreClass-class
#' @aliases scoreClass
#' @docType class
#' @title S4 class for representing model score items
#' @slot notation
#' @slot name
#' @slot explanation
#' @slot values
#' @rdname scoreClass-class
#' @exportClass scoreClass
#' @examples
#'  \donttest{new("scoreClass")}

setClass(Class          = "scoreClass",
         representation = representation(notation    = "character",
                                         name        = "character",
                                         explanation = "character"),
         prototype      = prototype(notation    = "",
                                    name        = "",
                                    explanation = ""))

#' Show method for \code{\linkS4class{scoreClass}}
#'
#' @rdname show-methods
#' @aliases show, scoreClass-method
#' @docType methods
#' @title Show method for 'scoreClass'
#' @param object a \code{scoreClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("scoreClass"))
#'
#' new(Class="scoreClass",
#' notation="U1",
#' name="Plausibility",
#' values=c(0,1,2,3),
#' explanation=gsub(x="1=high (highly plausible despite absence of factual
#' evidence), 2=medium (plausible despite absence of factual evidence),
#' 3=low (plausibility questionable or not assessed;
#' fictive or speculative assertion)","\n",replacement=""))}

setMethod(f          = "show",
          signature  = signature(object = "scoreClass"),
          definition = function(object) { 
            cat("Notation: ", object@notation, "\n")
            cat("Name: ",     object@name, "\n")
            cat("Explanation: ", putLineBreaks(object@explanation), "\n")  
          })

#' toList method for \code{\linkS4class{scoreClass}}
#'
#' @rdname toList-methods
#' @aliases toList, scoreClass-method
#' @docType methods
#' @title toList method for 'scoreClass'
#' @param object a \code{scoreClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#'  \donttest{toList(new("scoreClass"))
#'
#' new(Class="scoreClass",
#' notation="U1",
#' name="Plausibility",
#' values=c(0,1,2,3),
#' explanation=gsub(x="1=high (highly plausible despite absence of factual
#' evidence), 2=medium (plausible despite absence of factual evidence),
#' 3=low (plausibility questionable or not assessed;
#' fictive or speculative assertion)","\n",replacement=""))}

setMethod(f          = "toList", 
          signature  = signature(object = "scoreClass"), 
          definition = function(object) {
            list(notation    = object@notation, 
                 name        = object@name, 
                 explanation = object@explanation)})
