dynamic_require <- function(package){
  if (eval(parse(text = paste("require(",package,")")))) 
    return(TRUE)
  
  install.packages(package, repos = "http://cran.rstudio.com")
  return(eval(parse(text = paste("require(",package,")"))))
}

get_libraries <- function(packages) {
  for (package in packages)
    if (!dynamic_require(package)) 
      install.packages(package, repos = "http://cran.rstudio.com")
}

get_libraries(c("shiny"))

get_libraries(c("DT",
                "fitdistrplus",
                "mc2d",
                "network",
                "networkD3",
                "plotly",
                "rmarkdown", "knitr",
                "RJSONIO",
                #"shiny",
                "shinyalert",
                "shinycssloaders",
                "shinydashboard",
                "shinyFeedback",
                "shinyjs",
                #"parallel", 
                "mgcv"))

#options(warn = 2, show.error.locations = "bottom")
runApp('./inst/app.R', 
       port = 3838, 
       launch.browser = TRUE,
       display.mode = "normal")