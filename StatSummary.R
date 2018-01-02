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
# Functions
# ------------------------------------------------------------------------
summaryStats<-function(x) {
  n<-NROW(x)
  type = typeof(x)
  na <- sum(is.na(x))
  if (is.numeric(x) == TRUE) {
    median<-median(x)
    max<-max(x)
    min<-min (x)
    sd<- round(sd(x),3)
  } 
  summary<-list(
    type = type, 
    n = n,
    na = na,
    min = min,
    median = median,
    max = max,
    sd=sd
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

tbl <- summaryStatsTbl(mtcars)
tbl
str(tbl)
typeof(mtcars$cyl)

summaryStatsTbl(cars)
summaryStatsTbl(iris)       
