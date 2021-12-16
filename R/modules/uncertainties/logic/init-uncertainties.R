#' @name initUncertainties
#' @title initUncertainties
#' @description Initializes default uncertainties object.
#' @return default uncertainties object
#' @export
initUncertainties <- function() {
  cat("initUncertainties\n")

  um1 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Concept",
             explanation = "",
             scores      = c(1, 1, 1))

  um2 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Structure",
             explanation = "",
             scores     = c(1, 1, 1))

  um3 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Type",
             explanation = "",
             scores      = c(1, 1, 1))

  um4 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Resolution",
             explanation = "",
             scores      = c(1, 1, 1))

  um5 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Dependencies",
             explanation = "",
             scores      = c(1, 1, 1))

  um6 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Dose-response",
             explanation = "",
             scores      = c(1, 1, 1))

  um7 <- new(Class       = "uncertClass",
             namemain    = "modelling approach uncertainties",
             namesub     = "Outcome",
             explanation = "",
             scores      = c(1, 1, 1))
    
  us1 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Agents",
             explanation = "",
             scores      = c(1, 1, 1))

  us2 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Sources",
             explanation = "",
             scores      = c(1, 1, 1))

  us3 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Populations",
             explanation = "",
             scores      = c(1, 1, 1))

  us4 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Microenvironment",
             explanation = "",
             scores      = c(1, 1, 1))

  us5 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Time",
             explanation = "",
             scores      = c(1, 1, 1))

  us6 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Space",
             explanation = "",
             scores      = c(1, 1, 1))

  us7 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Measures",
             explanation = "",
             scores      = c(1, 1, 1))

  us8 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Activities",
             explanation = "",
             scores      = c(1, 1, 1))

  us9 <- new(Class       = "uncertClass",
             namemain    = "scenario uncertainties",
             namesub     = "Sources",
             explanation = "",
             scores      = c(1, 1, 1))

  us10 <- new(Class       = "uncertClass",
              namemain    = "scenario uncertainties",
              namesub     = "Pathways",
              explanation = "",
              scores      = c(1, 1, 1))

  ue1 <- new(Class       = "uncertClass",
             namemain    = "other uncertainties",
             namesub     = "example1",
             explanation = "",
             scores      = c(1, 1, 1))

  ue2 <- new(Class       = "uncertClass",
             namemain    = "other uncertainties",
             namesub     = "example2",
             explanation = "",
             scores      = c(1, 1, 1))

  uncertainties <- new(Class = "modelUncertaintiesClass",
                       note = "",
                       uncertainties = list(um1,
                                            um2,
                                            um3,
                                            um4,
                                            um5,
                                            um6,
                                            um7,
                                            us1,
                                            us2,
                                            us3,
                                            us4,
                                            us5,
                                            us6,
                                            us7,
                                            us8,
                                            us9,
                                            us10,
                                            ue1,
                                            ue2))

  uncertainties
}
