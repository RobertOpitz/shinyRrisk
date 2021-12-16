# definition of "authorClass"
# methods: "show", "toList"
#' S4 class represents information of an author of a \code{rrisk} model
#'
#' @name authorClass-class
#' @aliases authorClass
#' @docType class
#' @title S4 class for representing author's information
#' @slot name name
#' @slot institution institution
#' @slot email email address
#' @rdname authorClass-class
#' @exportClass authorClass
#' @examples
#' \donttest{new("authorClass")
#'
#' new("authorClass",name="Max Mustermann",institution="Example institution",
#' email="example@@email.com")}

setClass(Class          = "authorClass",
         representation = representation(name        = "character",
                                         institution = "character",
                                         email       = "character"),
         prototype      = prototype(name        = "",
                                    institution = "",
                                    email       = ""))

#' Show method for \code{\linkS4class{authorClass}}
#'
#' @rdname show-methods
#' @aliases show,authorClass-method
#' @docType methods
#' @title Show method for 'authorClass'
#' @param object a \code{authorClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#' \donttest{show(new("authorClass")) }

setMethod(f          = "show",
          signature  = signature(object = "authorClass"),
          definition = function(object) { 
            cat("Name: ", object@name, "\n")
            cat("Institution: ", object@institution, "\n")
            cat("Email: ", object@email, "\n")
          })

#' toList method for \code{\linkS4class{authorClass}}
#'
#' @rdname toList-methods
#' @aliases toList, authorClass-method
#' @docType methods
#' @title toList method for 'authorClass'
#' @param object a \code{authorClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#' \donttest{toList(new("authorClass")) }

setMethod(f          = "toList", 
          signature  = signature(object = "authorClass"), 
          definition = function(object) list(name        = object@name, 
                                             institution = object@institution, 
                                             email       = object@email))
