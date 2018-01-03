# ------------------------------------------------------------------------
# Description: Statistical summary table
# Link:
#
# References:
#   https://stackoverflow.com/questions/28144722/more-comprehensive-summary-function-in-r
#
# Location: /Users/raymondtse/Dropbox/Analysis/R Code/StatSummary.r
# First created: 23:42 - Tuesday 2 January 2018
# Last modified: 23:42 - Tuesday 2 January 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(tidyverse)

# ------------------------------------------------------------------------
# Functions to summarise a table of data
# ------------------------------------------------------------------------

# Need to test
initialiseVars <- function(...) {
  return(NULL)
}

summaryStats <- function(aVector) {
# initialiseVars(n, type, na, min, median, max, sd) # test function first
  n <- nrow(aVector)
  type <- typeof(aVector)
  na <- sum(is.na(aVector))
  if (is.numeric(aVector) == TRUE) {
    min <- min(aVector)
    median <- median(aVector)
    max <- max(aVector)
    sd <- round(sd(aVector),3)
  } 
  summary <- list(
    type = type, 
    n = n,
    na = na,
    min = min,
    median = median,
    max = max,
    sd = sd
  )
  return(summary)
}

summaryStatsTbl <- function(aTable) {
  tbl <- data.frame()
  for (i in seq_along(aTable)) {
    cols <- data.frame(summaryStats(aTable[[i]]))
    newRow <- data.frame("name" = colnames(aTable[i]))
    newRow <- bind_cols(newRow, cols)
  
    tbl <- bind_rows(tbl, newRow)
  }
  return(tbl)
}
# work out how to suppress warnings
# factors - number of levels, most common level
# characters - min length, max length, median length
# logical - true, false
# test cases and harness

tbl <- summaryStatsTbl(mtcars)
tbl
str(tbl)
typeof(mtcars$cyl)

summaryStatsTbl(cars)
summaryStatsTbl(iris)       
