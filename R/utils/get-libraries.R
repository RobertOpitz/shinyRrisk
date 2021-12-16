get_libraries <- function(packages) {
  for (package in packages)
    if (!require(package)) install.packages(package)
}