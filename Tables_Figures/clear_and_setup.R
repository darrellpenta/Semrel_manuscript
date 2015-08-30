rm(list = ls()) 
library(languageR) 
library(xlsx)

source(file = "semrel_ratings_data_read_in_dataframe.R")
get_stars = function( p) {
  stars = findInterval( p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("***" , "**","*", ".", " ")
  codes[stars]
}
options( scipen=1)

line = rep(c("-"), times = 40, fill = 80)
br   = "\n"