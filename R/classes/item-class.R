# definition of "itemClass"
# methods: "show", "toList"
#' S4 class for representing an Item of \code{rrisk} models
#'
#' @name itemClass-class
#' @aliases itemClass
#' @docType class
#' @title S4 class for representing model items
#' @slot part
#' @slot name
#' @slot title
#' @slot stratum
#' @slot explanation
#' @slot type
#' @slot typecode
#' @slot stratumevaluated
#' @slot data
#' @slot definition
#' @slot depitem
#' @slot unit
#' @slot role
#' @slot rolecode
#' @slot plausimin
#' @slot plausimax
#' @slot scores
#' @slot assumptions
#' @slot remark
#' @slot reference
#' @slot fullc
#' @slot relaxc
#' @slot fullcommand
#' @rdname itemClass-class
#' @exportClass itemClass
#' @examples
#' \donttest{new("itemClass")}

setClass(Class          = "itemClass",
         representation = representation(part             = "character",
                                         name             = "character",
                                         title            = "character",
                                         stratum          = "character",
                                         explanation      = "character",
                                         type             = "character",
                                         typecode         = "character",
                                         stratumevaluated = "ANY",
                                         data             = "ANY",
                                         definition       = "character",
                                         depitem          = "character",
                                         unit             = "character",
                                         role             = "character",
                                         rolecode         = "character",
                                         plausimin        = "numericNULL",
                                         plausimax        = "numericNULL",
                                         scores           = "numericNULL",
                                         assumptions      = "character",
                                         remark           = "character",
                                         reference        = "character",
                                         fullc            = "character",
                                         relaxc           = "character",                               
                                         fullcommand      = "numericNULL", 
                                         relaxcommand     = "numericNULL"),
         prototype      = prototype(part             = "",
                                    name             = "",
                                    title            = "",
                                    stratum          = "",
                                    stratumevaluated = "",
                                    explanation      = "",
                                    type             = "",
                                    typecode         = "",
                                    data             = NULL,
                                    definition       = "",
                                    depitem          = "",
                                    unit             = "",
                                    role             = "Not defined (nd)",
                                    rolecode         = "nd",
                                    plausimin        = NULL,
                                    plausimax        = NULL,
                                    scores           = NULL,
                                    assumptions      = "",
                                    remark           = "",
                                    reference        = "",
                                    fullc            = "",
                                    relaxc           = "", 
                                    fullcommand      = NULL, 
                                    relaxcommand     = NULL))

#' Show method for \code{\linkS4class{itemClass}}
#'
#' @rdname show-methods
#' @aliases show, itemClass-method
#' @docType methods
#' @title Show method for 'itemClass'
#' @param object a \code{itemClass} object
#' @exportMethod show
##' @importFrom methods show
#' @examples
#'  \donttest{show(new("itemClass"))}

setMethod(f          = "show",
          signature  = signature(object = "itemClass"),
          definition = function(object) { 
            cat("Name:\t\t", object@name, "\n") 
            cat("Part:\t\t", object@part, "\n") 
            cat("Title:\t\t", object@title, "\n")
            cat("Stratum:\t", object@stratum, "\n")
            cat("Type:\t\t", object@type, "\n")
            cat("Type code:\t", object@typecode, "\n")
            if (object@typecode == "stra") 
              cat("Stratum evaluated:\t", object@stratumevaluated, "\n")
            cat("Data:\n")
            if (!is.null(object@data) & object@typecode == "numv")
              print(object@data)
            else if (!is.null(object@data) & object@typecode == "mcrv")
              print(object@data)
            else if (!is.null(object@data) & object@typecode == "fnrv")
              print(object@data) 
            else if (!is.null(object@data) & object@typecode == "stra")
              print(object@data)
            else if (!is.null(object@data) & object@typecode == "rsrv")
              print(object@data)
            else if(!is.null(object@data) & object@typecode == "bsrv")
              print(object@data)
            else if (!is.null(object@data) & object@typecode == "data") {
              print(head(object@data))
              cat("...\n")
              print(summary(object@data))
            } else if (!is.null(object@data) & object@typecode == "fdoc")
              print(object@data)
            else if (!is.null(object@data) & object@typecode == "bdjp")
              print(object@data)
            else if (!is.null(object@data) & object@typecode == "mxrv")
              print(object@data)
            cat("Definition:\t", object@definition, "\n")
            cat("Dependent items:", object@depitem, "\n")
            cat("Unit:\t\t", object@unit, "\n")
            cat("Role:\t\t", object@role, "\n")
            cat("Role code:\t", object@rolecode, "\n")
            cat(paste0("Plausimin:\t", object@plausimin, "\n"))
            cat(paste0("Plausimax:\t", object@plausimax, "\n"))
            if (is.null(object@scores))
              cat("Scores:\t\t\n")
            else 
              cat("Scores:\t\t", object@scores, "\n")
            cat("Explanation:\t", putLineBreaks(object@explanation), "\n")
            cat("Assumptions:\t", putLineBreaks(object@assumptions), "\n")
            cat("Remark:\t\t", putLineBreaks(object@remark), "\n")
            cat("Reference:\t", object@reference, "\n")
            cat(paste0("Full command:\t", object@fullc, "\n"))
            cat(paste0("Relax command:\t", object@relaxc, "\n"))
          })

#' toList method for \code{\linkS4class{itemClass}}
#'
#' @rdname toList-methods
#' @aliases toList, itemClass-method
#' @docType methods
#' @title toList method for 'itemClass'
#' @param object a \code{itemClass} object
#' @exportMethod toList
##' @importFrom methods toList
#' @examples
#'  \donttest{toList(new("itemClass"))}
setMethod(f          = "toList", 
          signature  = signature(object = "itemClass"), 
          definition = function(object) {
            list(name       = object@name, 
                 typecode   = object@typecode, 
                 definition = object@definition, 
                 depitem    = object@depitem)})
