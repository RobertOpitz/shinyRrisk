#' S4 virtual class, a union of classes \code{numeric} and \code{NULL}.
#'
#' @name numericNULL-class
#' @aliases numericNULL
#' @docType class
#' @title S4 virtual class
#' @rdname numericNULL-class
#' @exportClass numericNULL

setClassUnion("numericNULL", c("numeric", "NULL"))