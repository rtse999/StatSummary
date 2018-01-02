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
  median<-median(x)
  max<-max(x)
  min<-min (x)
  sd<- round(sd(x),3)
  na <- sum(is.na(x))
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

tbl <- data.frame()
for (i in seq_along(mtcars)) {
  cols <- data.frame(summaryStats(mtcars[[i]]))
  newRow <- data.frame("name" = colnames(mtcars[i]))
  newRow <- bind_cols(newRow, cols)
  
  tbl <- bind_rows(tbl, newRow)
}
# work out how to suppress warnings
tbl
str(tbl)
typeof(mtcars$cyl)

       