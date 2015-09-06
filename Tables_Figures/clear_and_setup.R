rm(list = ls()) 
library(xlsx)

get_stars = function( p) {
  stars = findInterval( p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("***" , "**","*", ".", " ")
  codes[stars]
}
options( scipen=1)

line = rep(c("-"), times = 40, fill = 80)
br   = "\n"

get_range = function( p) {
  range = findInterval( p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("<.001" , "<.01","<.05", "<.10", ">.10")
  codes[range]
}
options( scipen=1)

get_range.tex = function( p) {
  range = findInterval( p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("\\textless.001" , "\\textless.01","\\textless.05", "\\textless.10", "\\textgreater.10")
  codes[range]
}
options( scipen=1)
