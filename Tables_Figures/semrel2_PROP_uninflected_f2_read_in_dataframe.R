# -------------------
library(reshape2)
library(plyr)
library(stringr) 
library(dplyr)

d.base           <- read.csv("data/SR1&2_coding.csv", stringsAsFactors = FALSE )
d.prop            <- subset(d.base, subexpt == "Prop")
d.prop            <- subset(d.prop,  subject != "1031")
d.prop            <- subset(d.prop,  subject != "1123")


d.prop            <- d.prop[, c(3,7:8,15:16)]
d.prop$unique.id <- paste(d.prop[,1], d.prop[,2],d.prop[,3], sep = "_")

d.prop.un           <- ddply(d.prop, "unique.id", function(X) data.frame(unind = sum(X$unind)))
d.prop.ms           <- ddply(d.prop, "unique.id", function(X) data.frame(misc = sum(X$misc)))


unind          <- data.frame(d.prop.un$unind)
misc           <- data.frame(d.prop.ms$misc)
d.prop.un        <- colsplit(d.prop.un$unique.id, "_", c("item", "related", "n2num"))
d.prop.ms        <- colsplit(d.prop.ms$unique.id, "_", c("item", "related", "n2num"))

d.prop.un$item      <- as.factor(d.prop.un$item)
d.prop.ms$item      <- as.factor(d.prop.ms$item)

d.prop.un           <- cbind(d.prop.un,unind)
colnames(d.prop.un)[4] <- "unind"
d.prop.ms           <- cbind(d.prop.ms,misc)
colnames(d.prop.ms)[4] <- "misc"
d.prop.un           <-merge(d.prop.un, d.prop.ms)
d.prop              <- d.prop.un
rm(unind)
rm(misc)


d.prop$item       <-as.factor(d.prop$item)
d.prop$related    <-as.factor(d.prop$related)
d.prop$n2num      <-as.factor(d.prop$n2num)
# Calculates the error rates (percent, including dys)

# 
# #aggregates d with dysfluencies
# # Below, designates various subsets of the original data file
# integ <- subset(data.item, semint   ==  "integ")
# unint <- subset(data.item, semint   ==  "unint")
# relat <- subset(data.item, related  ==  "rel")
# unrel <- subset(data.item, related  ==  "unrel")
# sing  <- subset(data.item, n2num    ==  "sing")
# plur  <- subset(data.item, n2num    ==  "plur")
# 
# #Below, additional subsetted groups
# relat.int.plur   <- subset(data.item, related == "rel"   & semint  == "integ" & n2num == "plur")
# relat.int.sing   <- subset(data.item, related == "rel"   & semint  == "integ" & n2num == "sing")
# relat.unint.plur <- subset(data.item, related == "rel"   & semint  == "unint" & n2num == "plur")
# relat.unint.sing <- subset(data.item, related == "rel"   & semint  == "unint" & n2num == "sing")
# unrel.int.plur   <- subset(data.item, related == "unrel" & semint  == "integ" & n2num == "plur")
# unrel.int.sing   <- subset(data.item, related == "unrel" & semint  == "integ" & n2num == "sing")
# unrel.unint.plur <- subset(data.item, related == "unrel" & semint  == "unint" & n2num == "plur")
# unrel.unint.sing <- subset(data.item, related == "unrel" & semint  == "unint" & n2num == "sing")
# relat.plur       <- subset(data.item, related == "rel"   & n2num   == "plur")
# relat.sing       <- subset(data.item, related == "rel"   & n2num   == "sing")
# unrel.plur       <- subset(data.item, related == "unrel" & n2num   == "plur")
# unrel.sing       <- subset(data.item, related == "unrel" & n2num   == "sing")
# integ.plur       <- subset(data.item, semint  == "integ" & n2num   == "plur")
# integ.sing       <- subset(data.item, semint  == "integ" & n2num   == "sing")
# unint.plur       <- subset(data.item, semint  == "unint" & n2num   == "plur")
# unint.sing       <- subset(data.item, semint  == "unint" & n2num   == "sing")
# integ.relat      <- subset(data.item, semint  == "integ" & related == "rel")
# integ.unrel      <- subset(data.item, semint  == "integ" & related == "unrel")
# unint.relat      <- subset(data.item, semint  == "unint" & related == "rel")
# unint.unrel      <- subset(data.item, semint  == "unint" & related == "unrel")
# 
# 
# ds <- data.frame(data = c(
#   "gmean",
#   "integ",
#   "unint",
#   "relat",
#   "unrel",
#   "integrel",
#   "integunrel",
#   "unintrel",
#   "unintunrel",
#   "plur",
#   "sing",
#   "intplur",
#   "intsing",
#   "unintplur",
#   "unintsing",
#   "relplur",
#   "relsing",
#   "unrelplur",
#   "unrelsing",
#   "relintplur",
#   "relintsing",
#   "relunintplur",
#   "relunintsing",
#   "unrelintplur",
#   "unrelintsing",
#   "unrelunintplur",
#   "unrelunintsing"),
#   
#   n = c(length(data.item$unind),
#         length(integ$unind),
#         length(unint$unind),
#         length(relat$unind),
#         length(unrel$unind),
#         length(integ.relat$unind),
#         length(integ.unrel$unind),
#         length(unint.relat$unind),
#         length(unint.unrel$unind),
#         length(plur$unind),
#         length(sing$unind),
#         length(integ.plur$unind),
#         length(integ.sing$unind),
#         length(unint.plur$unind),
#         length(unint.sing$unind),
#         length(relat.plur$unind),
#         length(relat.sing$unind),
#         length(unrel.plur$unind),
#         length(unrel.sing$unind),
#         length(relat.int.plur$unind),
#         length(relat.int.sing$unind),
#         length(relat.unint.plur$unind),
#         length(relat.unint.sing$unind),
#         length(unrel.int.plur$unind),
#         length(unrel.int.sing$unind),
#         length(unrel.unint.plur$unind),
#         length(unrel.unint.sing$unind)),
#   
#   N = c(length(data.item$unind),
#         length(integ$unind),
#         length(unint$unind),
#         length(relat$unind),
#         length(unrel$unind),
#         length(integ.relat$unind),
#         length(integ.unrel$unind),
#         length(unint.relat$unind),
#         length(unint.unrel$unind),
#         length(plur$unind),
#         length(sing$unind),
#         length(integ.plur$unind),
#         length(integ.sing$unind),
#         length(unint.plur$unind),
#         length(unint.sing$unind),
#         length(relat.plur$unind),
#         length(relat.sing$unind),
#         length(unrel.plur$unind),
#         length(unrel.sing$unind),
#         length(relat.int.plur$unind),
#         length(relat.int.sing$unind),
#         length(relat.unint.plur$unind),
#         length(relat.unint.sing$unind),
#         length(unrel.int.plur$unind),
#         length(unrel.int.sing$unind),
#         length(unrel.unint.plur$unind),
#         length(unrel.unint.sing$unind)),
#   
#   mean = c(mean(data.item$unind),
#            mean(integ$unind),
#            mean(unint$unind),
#            mean(relat$unind),
#            mean(unrel$unind),
#            mean(integ.relat$unind),
#            mean(integ.unrel$unind),
#            mean(unint.relat$unind),
#            mean(unint.unrel$unind),
#            mean(plur$unind),
#            mean(sing$unind),
#            mean(integ.plur$unind),
#            mean(integ.sing$unind),
#            mean(unint.plur$unind),
#            mean(unint.sing$unind),
#            mean(relat.plur$unind),
#            mean(relat.sing$unind),
#            mean(unrel.plur$unind),
#            mean(unrel.sing$unind),
#            mean(relat.int.plur$unind),
#            mean(relat.int.sing$unind),
#            mean(relat.unint.plur$unind),
#            mean(relat.unint.sing$unind),
#            mean(unrel.int.plur$unind),
#            mean(unrel.int.sing$unind),
#            mean(unrel.unint.plur$unind),
#            mean(unrel.unint.sing$unind)),
#   
#   sd = c(sd(data.item$unind),
#          sd(integ$unind),
#          sd(unint$unind),
#          sd(relat$unind),
#          sd(unrel$unind),
#          sd(integ.relat$unind),
#          sd(integ.unrel$unind),
#          sd(unint.relat$unind),
#          sd(unint.unrel$unind),
#          sd(plur$unind),
#          sd(sing$unind),
#          sd(integ.plur$unind),
#          sd(integ.sing$unind),
#          sd(unint.plur$unind),
#          sd(unint.sing$unind),
#          sd(relat.plur$unind),
#          sd(relat.sing$unind),
#          sd(unrel.plur$unind),
#          sd(unrel.sing$unind),
#          sd(relat.int.plur$unind),
#          sd(relat.int.sing$unind),
#          sd(relat.unint.plur$unind),
#          sd(relat.unint.sing$unind),
#          sd(unrel.int.plur$unind),
#          sd(unrel.int.sing$unind),
#          sd(unrel.unint.plur$unind),
#          sd(unrel.unint.sing$unind)),
#   
#   se = c(sd(data.item$unind) / sqrt(length(data.item$unind)),
#          sd(integ$unind) / sqrt(length(integ$unind)),
#          sd(unint$unind) / sqrt(length(unint$unind)),
#          sd(relat$unind) / sqrt(length(relat$unind)),
#          sd(unrel$unind) / sqrt(length(unrel$unind)),
#          sd(integ.relat$unind) / sqrt(length(integ.relat$unind)),
#          sd(integ.unrel$unind) / sqrt(length(integ.unrel$unind)),
#          sd(unint.relat$unind) / sqrt(length(unint.relat$unind)),
#          sd(unint.unrel$unind) / sqrt(length(unint.unrel$unind)),
#          sd(plur$unind) / sqrt(length(plur$unind)),
#          sd(sing$unind) / sqrt(length(sing$unind)),
#          sd(integ.plur$unind) / sqrt(length(integ.plur$unind)),
#          sd(integ.sing$unind) / sqrt(length(integ.sing$unind)),
#          sd(unint.plur$unind) / sqrt(length(unint.plur$unind)),
#          sd(unint.sing$unind) / sqrt(length(unint.sing$unind)),
#          sd(relat.plur$unind) / sqrt(length(relat.plur$unind) ),
#          sd(relat.sing$unind) / sqrt(length(relat.sing$unind)),
#          sd(unrel.plur$unind) / sqrt(length(unrel.plur$unind)),
#          sd(unrel.sing$unind) / sqrt(length(unrel.sing$unind)),
#          sd(relat.int.plur$unind) / sqrt(length(relat.int.plur$unind)),
#          sd(relat.int.sing$unind) / sqrt(length(relat.int.sing$unind)),
#          sd(relat.unint.plur$unind) / sqrt(length(relat.unint.plur$unind)),
#          sd(relat.unint.sing$unind) / sqrt(length(relat.unint.sing$unind)),
#          sd(unrel.int.plur$unind) / sqrt(length(unrel.int.plur$unind)),
#          sd(unrel.int.sing$unind) / sqrt(length(unrel.int.sing$unind)),
#          sd(unrel.unint.plur$unind) / sqrt(length(unrel.unint.plur$unind)),
#          sd(unrel.unint.sing$unind) / sqrt(length(unrel.unint.sing$unind))
#   ))
# 
