#' @name shinyAlert
#' @title shinyAlert
#' @description Prompts an alert box.
#' @param title Alter title.
#' @param message Alert message.
#' @param html Render message as HTML?
#' @param type Alert type.
#' @param classes Set of classes.
#' @return NULL
#' @export
shinyAlert <- function(title, message = "", html = FALSE, 
                       type = "success", classes = "") {
  cat("shinyAlert\n")
  shinyalert(title               = title,
             text                = message,
             size                = "s", 
             closeOnEsc          = TRUE,
             closeOnClickOutside = FALSE,
             html                = html,
             type                = type,
             showConfirmButton   = TRUE,
             showCancelButton    = FALSE,
             confirmButtonText   = "Okay",
             confirmButtonCol    = "#AEDEF4",
             timer               = 0,
             imageUrl            = "",
             animation           = TRUE,
             class               = classes)
}
