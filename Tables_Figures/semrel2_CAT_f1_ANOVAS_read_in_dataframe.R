


# ====================================================================================================
# ------------------------------------CATEGORY COORDINATE ITEMS ANALYSES
# ====================================================================================================
#
# -----------------------------------PREPARE DATA FILE FOR ANALYSES---------------------------------
#
f1errout <- read.table("data/SR2_F1_errcat.txt", header = T) # reads in all data from data file

d <- f1errout # renames data file

d$subj <- as.factor(d$subj) # designates "subject" as a factor

# Calculates the error rates (percent, including dys)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)

# aggregates d with dysfluencies

data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)

colnames(data.subj) <- c("subj", "related", "n2num", "error") # renames columns

# Below, designates various subsets of the original data file
relat       <- subset(data.subj, related  ==  "rel")
unrel       <- subset(data.subj, related  ==  "unrel")
sing        <- subset(data.subj, n2num    ==  "sing")
plur        <- subset(data.subj, n2num    ==  "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")



ds <- data.frame(data = c(
  "gmean",
  "relat",
  "unrel",
  "plur",
  "sing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing"),
  
  n = c(length(data.subj$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),
  
  N = c(length(data.subj$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),
  
  mean = c(mean(data.subj$error),
           mean(relat$error),
           mean(unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error)),
  
  sd = c(sd(data.subj$error),
         sd(relat$error),
         sd(unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error)),
  
  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error) ),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
  ))


