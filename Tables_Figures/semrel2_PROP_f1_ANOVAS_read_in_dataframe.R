
# # ====================================================================================================
# # --------------------------------------PROPERTY ITEMS ANALYSES---------------------
# # ====================================================================================================

f1errout <- read.table("data/SR2_F1_errprop.txt", header = T)
d <- f1errout
d$subj <- as.factor(d$subj)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.subj <- aggregate(d$pct, list(d$subj, d$related, d$n2num ), mean)
colnames(data.subj) <- c("subj", "related", "n2num", "error")
relat       <- subset(data.subj, related == "rel")
assoc       <- subset(data.subj, related == "assoc")
unrel       <- subset(data.subj, related == "unrel")
sing        <- subset(data.subj, n2num   == "sing")
plur        <- subset(data.subj, n2num   == "plur")
relat.plur  <- subset(data.subj, related == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.subj, related == "rel"   & n2num   == "sing")
assoc.plur  <- subset(data.subj, related == "assoc" & n2num   == "plur")
assoc.sing   <- subset(data.subj, related == "assoc" & n2num   == "sing")
unrel.plur  <- subset(data.subj, related == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.subj, related == "unrel" & n2num   == "sing")


ds <- data.frame(data = c(
  "gmean",
  "assoc",
  "relat",
  "unrel",
  "plur",
  "sing",
  "assocplur",
  "assocsing",
  "relplur",
  "relsing",
  "unrelplur",
  "unrelsing"),
  
  n = c(length(data.subj$error),
        length(assoc$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),
  
  N = c(length(data.subj$error),
        length(assoc$error),
        length(relat$error),
        length(unrel$error),
        length(plur$error),
        length(sing$error),
        length(assoc.plur$error),
        length(assoc.sing$error),
        length(relat.plur$error),
        length(relat.sing$error),
        length(unrel.plur$error),
        length(unrel.sing$error)),
  
  mean = c(mean(data.subj$error),
           mean(assoc$error),
           mean(relat$error),
           mean(unrel$error),
           mean(plur$error),
           mean(sing$error),
           mean(assoc.plur$error),
           mean(assoc.sing$error),
           mean(relat.plur$error),
           mean(relat.sing$error),
           mean(unrel.plur$error),
           mean(unrel.sing$error)),
  
  sd = c(sd(data.subj$error),
         sd(assoc$error),
         sd(relat$error),
         sd(unrel$error),
         sd(plur$error),
         sd(sing$error),
         sd(assoc.plur$error),
         sd(assoc.sing$error),
         sd(relat.plur$error),
         sd(relat.sing$error),
         sd(unrel.plur$error),
         sd(unrel.sing$error)),
  
  se = c(sd(data.subj$error) / sqrt(length(data.subj$error)),
         sd(assoc$error) / sqrt(length(assoc$error)),
         sd(relat$error) / sqrt(length(relat$error)),
         sd(unrel$error) / sqrt(length(unrel$error)),
         sd(plur$error) / sqrt(length(plur$error)),
         sd(sing$error) / sqrt(length(sing$error)),
         sd(assoc.plur$error) / sqrt(length(assoc.plur$error)),
         sd(assoc.sing$error) / sqrt(length(assoc.sing$error)),
         sd(relat.plur$error) / sqrt(length(relat.plur$error)),
         sd(relat.sing$error) / sqrt(length(relat.sing$error)),
         sd(unrel.plur$error) / sqrt(length(unrel.plur$error)),
         sd(unrel.sing$error) / sqrt(length(unrel.sing$error))
  ))


