#' @description Function rudiscrete
#'
#' @details This function is not intended to be called directly but is internally called
#' during \code{rrisk} session.
#'
#' @name rudiscrete
#' @aliases rudiscrete
#' @title rudiscrete
#' @usage rudiscrete(n=100,min,max)
#' @param n n
#' @param min min
#' @param max max
#' @export
#' @keywords manip
rudiscrete <- function(n = 100, min, max) {
  cat("rudiscrete\n")
  output <- NULL
  for(j in seq_len(n/length(min))) {
    output <- c(output,
                sapply(seq_along(min),
                       function(i) sample(min[i]:max[i],
                                          size = 1,
                                          replace = TRUE)))
  }
  output
}

################################################################################
################################################################################
#' @description Function rdiscrete
#'
#' @details This function is not intended to be called directly but is
#' internally called during \code{rrisk} session.
#'
#' @name rdiscrete
#' @aliases rdiscrete
#' @title rdiscrete
#' @usage rdiscrete(n,values,probs)
#' @param n n
#' @param values values
#' @param probs probs
#' @export
#' @keywords manip
rdiscrete <- function(n = 100, values = seq_along(probs), probs) {
  cat("rdiscrete\n")
  sample(values, size = n, replace = TRUE, prob = probs)
}