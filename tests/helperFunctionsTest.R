library(assertthat)
source(paste0(getwd(), "/logic/helperFunctions.R"))

assert_that(printHelloWorld() == 1)
