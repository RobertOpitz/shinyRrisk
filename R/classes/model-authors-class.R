# definition of "modelAuthorsClass"
# methods: "show", "toList"
#' S4 class for representing information of all the authors of a \code{rrisk} models
#'
#' @name modelAuthorsClass-class
#' @aliases modelAuthorsClass
#' @docType class
#' @title S4 class for representing 'rrisk' model authors
#' @slot authors
#' @rdname modelAuthorsClass-class
#' @exportClass modelAuthorsClass
#' @examples
#' \donttest{new("modelAuthorsClass")
#'
#' new("modelAuthorsClass",authors=list(new("authorClass",name="Max Mustermann",
#' institution="Example institution",email="example@@email.com")))}

setClass(Class          = "modelAuthorsClass",
         representation = representation(authors = "list"),
         prototype      = prototype(authors = list()))

#' Show method for \code{\linkS4class{modelAuthorsClass}}
#'
#' @rdname show-methods
#' @aliases show,modelAuthorsClass-method
#' @docType methods
#' @title Show method for 'modelAuthorsClass'
#' @param object a \code{modelAuthorsClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("modelAuthorsClass"))}

setMethod(f          = "show",
          signature  = signature(object = "modelAuthorsClass"),
          definition = function(object) {
            cat("*************************************************************************\n")
            cat("Model authors\n")
            cat("*************************************************************************\n")
            if (length(object@authors) == 0)
              cat("Model authors list is empty \n\n")
            for (author in object@authors) {
              show(author)
              cat("\n")
            }
          })

#' toList method for \code{\linkS4class{modelAuthorsClass}}
#'
#' @rdname toList-methods
#' @aliases toList, modelAuthorsClass-method
#' @docType methods
#' @title toList method for 'modelAuthorsClass'
#' @param object a \code{modelAuthorsClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#' \donttest{toList(new("modelAuthorsClass"))}

setMethod(f          = "toList", 
          signature  = signature(object = "modelAuthorsClass"), 
          definition = function(object) lapply(object@authors, toList))
