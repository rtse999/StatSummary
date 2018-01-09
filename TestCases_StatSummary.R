# ------------------------------------------------------------------------
# Description: Test cases: Statistical summary table 
# Link:
#
# References:
#  https://stackoverflow.com/questions/28144722/more-comprehensive-summary-function-in-r
#
# Location: /Users/raymondtse/Dropbox/Analysis/R Code/TestCases_StatSummary.r 
# First created: 14:20 - Tuesday 9 January 2018
# Last modified: 14:20 - Tuesday 9 January 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")


# ------------------------------------------------------------------------
# Test scripts
# ------------------------------------------------------------------------
testRow1 <- NULL
testRow1 <- summaryStats(mtcars$mpg) # numeric field
testRow1

testRow2 <- NULL
testRow2 <- summaryStats(iris$Species) # factor field
testRow2

testTbl1 <- NULL
testTbl1 <- summaryStatsTbl(mtcars) # dataframe of all numeric columns
testTbl1

testTbl2 <- NULL
testTbl2 <- summaryStatsTbl(iris) # dataframe of numeric and factor columns
testTbl2

testTbl3 <- NULL
testTbl3 <- summaryStatsTbl(esoph) # can the function handle ordered factors ?
testTbl3

# ------------------------------------------------------------------------
# Backlog
# ------------------------------------------------------------------------
#
# Add test case for a vector with NA
#