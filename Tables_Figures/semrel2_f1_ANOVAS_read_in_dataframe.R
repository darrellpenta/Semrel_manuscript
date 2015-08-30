


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


#--------------------------------2 X 2 ANOVA------------------------------------------------------


sink("output/SemRel2 F1 Factorial Analyses.txt")
cat(" ", "\n")
cat("BY-SUBJECTS FACTORIAL ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: CATEGORY COORDINATES", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")

a.2x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.2x2))

cat(" ", "\n")
cat(" ", "\n")


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

#
# --------------------------------3 X 2  ANOVA------------------------------------------------------
#
cat(rep(c("-"), times=40, quote=F),"\n")
cat("2X2 ANOVA: PROPERTY ITEMS", sep = "", fill = 60)
cat(rep(c("-"), times=40, quote=F), "\n")
print(ds)
cat(" ", "\n")
a.3x2 <- aov(error ~ related * n2num + Error(subj / (related * n2num)), data = data.subj)
print(summary(a.3x2))
cat(" ", "\n")
cat(" ", "\n")



sink()

