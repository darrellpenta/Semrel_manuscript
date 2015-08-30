d.base           <- read.csv("data/SR1&2_coding.csv")

# -------------------- CAT CORD: SUBSETS  ---------
d.cat            <- subset(d.base, subexpt == "Cat")
d.cat            <- subset(d.cat,  subject != 1031 & subject != 1123)
d.cat$errcord    <- (d.cat$errd + d.cat$corrd)
d.cat$related    <- as.factor(d.cat$related)
d.cat$related    <- droplevels(d.cat$rel)
d.cat$n2num      <- as.factor(d.cat$n2num)
d.cat$item       <- as.factor(d.cat$item)
d.cat$subject    <- as.factor(d.cat$subject)

# Below, designates various subsets of the original data file
relat <- subset(d.cat, related      ==  "rel")
unrel <- subset(d.cat, related      ==  "unrel")
sing  <- subset(d.cat, n2num        ==  "sing")
plur  <- subset(d.cat, n2num        ==  "plur")

#Below, additional subsetted groups
relat.plur       <- subset(d.cat, related     == "rel"   & n2num      == "plur")
relat.sing       <- subset(d.cat, related     == "rel"   & n2num      == "sing")
unrel.plur       <- subset(d.cat, related     == "unrel" & n2num      == "plur")
unrel.sing       <- subset(d.cat, related     == "unrel" & n2num      == "sing")
