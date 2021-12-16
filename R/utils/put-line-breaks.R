#' @description This function wraps a text in lines with definite length.
#'
#' @details The argument \code{object} denotes the text to be displayed. \cr
#' If \code{show.output=TRUE} the text will be displayed in lines with a definite
#' length, otherweise blank. \cr
#' The argument \code{width.output} denotes the to be defined length of a line
#'
#' @name putLineBreaks
#' @aliases putLineBreaks
#' @title function for wraping text by definite length
#' @author Matthias Greiner \email{matthias.greiner@@bfr.bund.de}(BfR), \cr
#' Katharina Schueller \email{schueller@@stat-up.de}(\acronym{STAT-UP} Statistical Consulting), \cr
#' Natalia Belgorodski \email{belgorodski@@stat-up.de}(\acronym{STAT-UP} statistical Consulting), \cr
#' Robert Opitz \email{robert.opitz@@bfr.bund.de}
#' @usage putLineBreaks(object,show.output=FALSE,width.output)
#' @param object character string, text to be wraped
#' @param show.output logical, if \code{TRUE} the text will be wraped in lines with definite length
#' @param width.output numeric, single positive integer defining the length of a line
#' @return Returns the wraped text in lines with user-defined length if
#' \code{show.output=TRUE}. Otherwise no text will be returned.
#' @keywords misc
#' @export
#' @examples
#' \donttest{object<-"This is a lang text to be wraped. This is a lang text
#'  to be wraped. This is a lang text to be wraped. This is a lang text to be
#'  wraped. This is a lang text to be wraped. This is a lang text to be wraped.
#'  This is a lang text to be wraped. This is a lang text to be wraped."
#' object<-gsub(x=object,"\n",replacement="")
#' putLineBreaks(object=object,show.output=TRUE)       }
putLineBreaks <- function(text, show.output = FALSE, width.output = 70) {
  cat("putLineBreaks\n")
  # split the text into words
  words <- strsplit(text, split = " ")[[1]]
  # max number of words in text
  # (this number will change, as \n-words are added)
  max_words <- length(words)
  # number of chars in one line
  chars_in_line <- 0
  # counter for the current word in words
  next_word <- 1
  # go through all the words
  while (next_word <= max_words) {
    # how many chars of words are in one line
    chars_in_line <- chars_in_line + nchar(words[next_word])
    if (chars_in_line >= width.output) {
      # has the line to many words, then take the last word to add a break
      # otherwise use the current word position to add the break
      if (chars_in_line > width.output)
        set_break_at <- next_word - 1
      else
        set_break_at <- next_word
      # add a break at break point
      words <- append(words, "\n", set_break_at)
      # reset counter for number of chars in line
      # as one character is added for space character anyway, set it to -1
      # so it becomes zero in the end
      chars_in_line <- -1
      # the words vector contains now one additional entry,
      # therefore the max number of entries in this vector must be
      # increased by one
      max_words <- max_words + 1
    }
    # go to the next word
    next_word <- next_word + 1
    # add one space charater between words
    chars_in_line <- chars_in_line + 1
  }
  
  if (show.output)
    cat(paste(words))
  
  invisible(words)
}