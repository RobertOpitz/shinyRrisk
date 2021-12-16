#' @name initScoreSystem
#' @title initScoreSystem
#' @description Initializes default scoring system object.
#' @return default scoring system object
#' @export
initScoreSystem <- function() {
  cat("initScoreSystem\n")

  score1 <- new(Class       = "scoreClass",
                notation    = "D",
                name        = "Degree",
                explanation = paste("Degree of uncertainty describes the", 
                                    "possible deviation of the assessment", 
                                    "result from the true situation."))
  
  score2 <- new(Class       = "scoreClass",
                notation    = "C",
                name        = "Confidence",
                explanation = paste("Confidence in the knowledge base", 
                                    "comprises the completeness of all",
                                    "available information that can be used", 
                                    "for the assessment."))
  
  score3 <- new(Class       = "scoreClass",
                notation    = "S",
                name        = "Subjectivity",
                explanation = paste("Subjectivity of choices comprises the", 
                                    "reasons for decisions with regard to the", 
                                    "assessment (based on knowledge and", 
                                    "opinions in the scientific community or", 
                                    "in the group of stakeholders)."))
  
  scoringSystem <- new(Class       = "modelScoringClass",
                       name        = "Default: Modified IPCS/IMOC scheme",
                       tableheader = "",
                       explanatory = paste('The default scoring system is', 
                                           'based on IPCS and IMOC (2008) as', 
                                           'adopted in the BfR Guidance on', 
                                           'Uncertainty Analysis', 
                                           '(Heinemeyer et al., 2015).', 
                                           'According to these Guidances,', 
                                           'three dimensions are considered:', 
                                           '"Degree of uncertainty",', 
                                           '"Confidence in the knowledge base"', 
                                           'and "Subjectivity of choices",', 
                                           'abbreviated D, C and S,', 
                                           'respectively. The D dimension is', 
                                           'assessed using a 13-level-scale', 
                                           '(see Table 9 in Heinemeyer et al.,', 
                                           '2015 and scoring values below)', 
                                           'while C and S are assessed using', 
                                           'a 3-level-scale (low-medium-high;', 
                                           'see Table 24 in EFSA et al., 2018).', 
                                           'The default rrisk scoring systems', 
                                           'maps the 13-level-scale against the', 
                                           '3-level-scale (see scoring values),', 
                                           'the latter being the rrisk standard', 
                                           'for colour coding. The default and', 
                                           'legitimate entry for all scoring', 
                                           'values is "not assessed" (NA).', 
                                           'Thus, the uncertainty scoring is a', 
                                           'sequence of three value (separated', 
                                           'by blank), one for each dimension', 
                                           'with allowed entries (0, 1, 2, …,', 
                                           '12, NA).'),
                       values      = 1:14,
                       vcolors     = getScoringColors(),
                       vmeanings   = getScoringMeaningsDetailed(),
                       systemtype  = "rrisk scoring system",
                       scoring     = list(score1,
                                          score2,
                                          score3))

  scoringSystem 
}
