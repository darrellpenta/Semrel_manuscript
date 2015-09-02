d.base           <- read.csv("data/SR1&2_coding.csv")
# ---------------------- PROP: SUBSETS  ---------
d.sr2            <- subset(d.base, subexpt != "SemRel")
d.sr2           <-  subset(d.sr2, subject !="1031")
d.sr2           <-  subset(d.sr2, subject !="1123")


d.sr2$errcord    <- (d.sr2$errd + d.sr2$corrd)
d.sr2$related    <- as.factor(d.sr2$related)
d.sr2$related    <- droplevels(d.sr2$rel)
d.sr2$n2num      <- as.factor(d.sr2$n2num)
d.sr2$item       <- as.factor(d.sr2$item)
d.sr2$subject       <- as.factor(d.sr2$subject)


# Below, designates various subsets of the original data file

attrb <- subset(d.sr2, related      == "rel")
assoc <- subset(d.sr2, related      == "assoc")
unrel <- subset(d.sr2, related      == "unrel")
sing  <- subset(d.sr2, n2num        == "sing")
plur  <- subset(d.sr2, n2num        == "plur")

#Below, additional subsetted groups

attrb.assoc      <- subset(d.sr2, related     != "unrel")
attrb.assoc.plur <- subset(d.sr2, related     != "unrel" & n2num == "plur")
attrb.assoc.sing <- subset(d.sr2, related     != "unrel" & n2num == "sing")
attrb.unrel      <- subset(d.sr2, related     != "assoc")
attrb.unrel.plur <- subset(d.sr2, related     != "assoc" & n2num == "plur")
attrb.unrel.sing <- subset(d.sr2, related     != "assoc" & n2num == "sing")
assoc.unrel      <- subset(d.sr2, related     != "rel")
assoc.unrel.plur <- subset(d.sr2, related     != "rel" & n2num == "plur")
assoc.unrel.sing <- subset(d.sr2, related     != "rel" & n2num == "sing")
attrb.plur       <- subset(d.sr2, related     == "rel"   & n2num   == "plur")
attrb.sing       <- subset(d.sr2, related     == "rel"   & n2num   == "sing")
assoc.plur       <- subset(d.sr2, related     == "assoc" & n2num   == "plur")
assoc.sing       <- subset(d.sr2, related     == "assoc" & n2num   == "sing")
unrel.plur       <- subset(d.sr2, related     == "unrel" & n2num   == "plur")
unrel.sing       <- subset(d.sr2, related     == "unrel" & n2num   == "sing")

