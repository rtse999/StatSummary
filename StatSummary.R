# ------------------------------------------------------------------------
# Description: Statistical summary table 
# Link:
#
# References:
#  https://stackoverflow.com/questions/28144722/more-comprehensive-summary-function-in-r
#
# Location: /Users/raymondtse/Dropbox/Analysis/R Code/StatSummary.r 
# First created: 23:42 - Tuesday 2 January 2018 
# Last modified: 14:21 - Tuesday 9 January 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(dplyr)

# ------------------------------------------------------------------------
# Functions to summarise a table of data 
# ------------------------------------------------------------------------
summaryStats <- function(aVector) {
 n <- NROW(aVector)
 type <- typeof(aVector)
 factor <- is.factor(aVector)
 na <- sum(is.na(aVector))
 min <- NA
 median <- NA
 max <- NA
 sd <- NA
 levels <- NA
 most <- NA

 if (is.numeric(aVector) == TRUE) {
   min <- min(aVector, na.rm=TRUE)
   median <- median(aVector, na.rm=TRUE)
   max <- max(aVector, na.rm=TRUE)
   sd <- round(sd(aVector),3)
 } else if (is.factor(aVector) == TRUE) {
   levels <- nlevels(aVector)
   most <- names(which.max(table(aVector)))
 }

 summary <- list(
   type = type, 
   factor = factor,
   n = n,
   na = na,
   min = min,
   median = median,
   max = max,
   sd = sd,
   levels = levels,
   most = most
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
# characters - min length, max length, median length 
# logical - true, false



