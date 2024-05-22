# R package skeleton 

rm(list = ls())

install.packages('devtools')
install.packages('roxygen2')
install.packages('usethis')

library(devtools)
library(roxygen2)
library(usethis)

devtools::create("WeatherTripPlanner")

setwd("/Users/JasminHagemann/Desktop/GitHub/hagemann-WeatherTripPlanner/WeatherTripPlanner/R")

usethis::use_ccby_license()

# 3. move your R file with your functions into the R directory of your package

# 4.1 Define which functions should be available to your users
# use "#' @export" for that, add it to your .R function file
# 4.2 Use the document() function to make the functions available
# to users

devtools::document()

# 5. Do your R functions depend on other packages?
# (1) mark those functions clearly in your R file, by adding the package name + :: 
# in front of your functions (e.g., ggplot2::ggplot())
# (2) add the packages to your dependencies in the Description file, e.g.,
# Imports:
#   ggplot2

# 6. build your package
# creates an installable file with ending "tar.gz"
devtools::build()

# 7. your package can now be installed (by you and others!)
rm(list = ls())
devtools::install()

