
#
# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f1errout <- read.table("data/SR_F1_errordata.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file

d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

#aggregates d with dysfluencies
data.subj <- aggregate(d$pct, list(d$subj, d$semint, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "semint", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
integ <- subset(data.subj, semint   ==  "integ")
unint <- subset(data.subj, semint   ==  "unint")
relat <- subset(data.subj, related  ==  "rel")
unrel <- subset(data.subj, related  ==  "unrel")
sing  <- subset(data.subj, n2num    ==  "sing")
plur  <- subset(data.subj, n2num    ==  "plur")

#Below, additional subsetted groups
relat.int.plur   <- subset(data.subj, related == "rel"   & semint  == "integ" & n2num == "plur")
relat.int.sing   <- subset(data.subj, related == "rel"   & semint  == "integ" & n2num == "sing")
relat.unint.plur <- subset(data.subj, related == "rel"   & semint  == "unint" & n2num == "plur")
relat.unint.sing <- subset(data.subj, related == "rel"   & semint  == "unint" & n2num == "sing")
unrel.int.plur   <- subset(data.subj, related == "unrel" & semint  == "integ" & n2num == "plur")
unrel.int.sing   <- subset(data.subj, related == "unrel" & semint  == "integ" & n2num == "sing")
unrel.unint.plur <- subset(data.subj, related == "unrel" & semint  == "unint" & n2num == "plur")
unrel.unint.sing <- subset(data.subj, related == "unrel" & semint  == "unint" & n2num == "sing")
relat.plur       <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing       <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur       <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing       <- subset(data.subj, related == "unrel" & n2num   == "sing")
integ.plur       <- subset(data.subj, semint  == "integ" & n2num   == "plur")
integ.sing       <- subset(data.subj, semint  == "integ" & n2num   == "sing")
unint.plur       <- subset(data.subj, semint  == "unint" & n2num   == "plur")
unint.sing       <- subset(data.subj, semint  == "unint" & n2num   == "sing")
integ.relat      <- subset(data.subj, semint  == "integ" & related == "rel")
integ.unrel      <- subset(data.subj, semint  == "integ" & related == "unrel")
unint.relat      <- subset(data.subj, semint  == "unint" & related == "rel")
unint.unrel      <- subset(data.subj, semint  == "unint" & related == "unrel")


ds <- data.frame(data = c(
  "gmean",
  "integ",
  "unint",
  "relat",
  "unrel",
  "integrel",
  "integunrel",
  "unintrel",
  "unintunrel",
  "plur",
  "sing",
  "intplur",
  "intsing",
  "unintplur",
  "unintsing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing",
  "relintplur",
  "relintsing",
  "relunintplur",
  "relunintsing",
  "unrelintplur",
  "unrelintsing",
  "unrelunintplur",
  "unrelunintsing"),
  
  n = c(length(data.subj$error),
        length(integ$error),
        length(unint$error),
        length(relat$error),
        length(unrel$error),
        length(integ.relat$error),
        length(integ.unrel$error),
        length(unint.relat$error),
        length(unint.unrel$error),
        length(plur$error),
        length(sing$error),
        length(integ.plur$error),
        length(integ.sing$error),
        length(unint.plur$error),
        length(unint.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error),
        length(relat.int.plur$error),
        length(relat.int.sing$error),
        length(relat.unint.plur$error),
        length(relat.unint.sing$error),
        length(unrel.int.plur$error),
        length(unrel.int.sing$error),
        length(unrel.unint.plur$error),
        length(unrel.unint.sing$error)),
  
  N = c(length(data.subj$error),
        length(integ$error),
        length(unint$error),
        length(relat$error),
        length(unrel$error),
        length(integ.relat$error),
        length(integ.unrel$error),
        length(unint.relat$error),
        length(unint.unrel$error),
        length(plur$error),
        length(sing$error),
        length(integ.plur$error),
        length(integ.sing$error),
        length(unint.plur$error),
        length(unint.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error),
        length(relat.int.plur$error),
        length(relat.int.sing$error),
        length(relat.unint.plur$error),
        length(relat.unint.sing$error),
        length(unrel.int.plur$error),
        length(unrel.int.sing$error),
        length(unrel.unint.plur$error),
        length(unrel.unint.sing$error)),
  
  mean = c(mean(data.subj$error),
           mean(integ$error),
           mean(unint$error),
           mean(relat$error),
           mean(unrel$error),
           mean(integ.relat$error),
           mean(integ.unrel$error),
           mean(unint.relat$error),
           mean(unint.unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(integ.plur$error),
           mean(integ.sing$error),
           mean(unint.plur$error),
           mean(unint.sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error),
           mean(relat.int.plur$error),
           mean(relat.int.sing$error),
           mean(relat.unint.plur$error),
           mean(relat.unint.sing$error),
           mean(unrel.int.plur$error),
           mean(unrel.int.sing$error),
           mean(unrel.unint.plur$error),
           mean(unrel.unint.sing$error)),
  
  sd = c(sd(data.subj$error),
         sd(integ$error),
         sd(unint$error),
         sd(relat$error),
         sd(unrel$error),
         sd(integ.relat$error),
         sd(integ.unrel$error),
         sd(unint.relat$error),
         sd(unint.unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(integ.plur$error),
         sd(integ.sing$error),
         sd(unint.plur$error),
         sd(unint.sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error),
         sd(relat.int.plur$error),
         sd(relat.int.sing$error),
         sd(relat.unint.plur$error),
         sd(relat.unint.sing$error),
         sd(unrel.int.plur$error),
         sd(unrel.int.sing$error),
         sd(unrel.unint.plur$error),
         sd(unrel.unint.sing$error)),
  
  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(integ$error) / sqrt(length(integ$error)),
         sd(unint$error) / sqrt(length(unint$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(integ.relat$error) / sqrt(length(integ.relat$error)),
         sd(integ.unrel$error) / sqrt(length(integ.unrel$error)),
         sd(unint.relat$error) / sqrt(length(unint.relat$error)),
         sd(unint.unrel$error) / sqrt(length(unint.unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(integ.plur$error) / sqrt(length(integ.plur$error)),
         sd(integ.sing$error) / sqrt(length(integ.sing$error)),
         sd(unint.plur$error) / sqrt(length(unint.plur$error)),
         sd(unint.sing$error) / sqrt(length(unint.sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error) ),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error)),
         sd(relat.int.plur$error) / sqrt(length(relat.int.plur$error)),
         sd(relat.int.sing$error) / sqrt(length(relat.int.sing$error)),
         sd(relat.unint.plur$error) / sqrt(length(relat.unint.plur$error)),
         sd(relat.unint.sing$error) / sqrt(length(relat.unint.sing$error)),
         sd(unrel.int.plur$error) / sqrt(length(unrel.int.plur$error)),
         sd(unrel.int.sing$error) / sqrt(length(unrel.int.sing$error)),
         sd(unrel.unint.plur$error) / sqrt(length(unrel.unint.plur$error)),
         sd(unrel.unint.sing$error) / sqrt(length(unrel.unint.sing$error))
  ))
