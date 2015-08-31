d.base           <- read.csv("data/SR1&2_coding.csv")

# ---------------------- PROP: SUBSETS  ---------
d.prop            <- subset(d.base, subexpt == "Prop")
d.prop            <- subset(d.prop,  subject != 1031)
d.prop            <- subset(d.prop,  subject != 1045)
d.prop            <- subset(d.prop,  subject != 1079)
d.prop            <- subset(d.prop,  subject != 1120)
d.prop            <- subset(d.prop,  subject != 1123)
d.prop$errcord    <- (d.prop$errd + d.prop$corrd)
d.prop$related    <- as.factor(d.prop$related)
d.prop$related    <- droplevels(d.prop$rel)
d.prop$n2num      <- as.factor(d.prop$n2num)
d.prop$item       <- as.factor(d.prop$item)
d.prop$subject       <- as.factor(d.prop$subject)


# Below, designates various subsets of the original data file

attrb <- subset(d.prop, related      == "rel")
assoc <- subset(d.prop, related      == "assoc")
unrel <- subset(d.prop, related      == "unrel")
sing  <- subset(d.prop, n2num        == "sing")
plur  <- subset(d.prop, n2num        == "plur")

#Below, additional subsetted groups

attrb.assoc      <- subset(d.prop, related     != "unrel")
attrb.assoc.plur <- subset(d.prop, related     != "unrel" & n2num == "plur")
attrb.assoc.sing <- subset(d.prop, related     != "unrel" & n2num == "sing")
attrb.unrel      <- subset(d.prop, related     != "assoc")
attrb.unrel.plur <- subset(d.prop, related     != "assoc" & n2num == "plur")
attrb.unrel.sing <- subset(d.prop, related     != "assoc" & n2num == "sing")
assoc.unrel      <- subset(d.prop, related     != "rel")
assoc.unrel.plur <- subset(d.prop, related     != "rel" & n2num == "plur")
assoc.unrel.sing <- subset(d.prop, related     != "rel" & n2num == "sing")
attrb.plur       <- subset(d.prop, related     == "rel"   & n2num   == "plur")
attrb.sing       <- subset(d.prop, related     == "rel"   & n2num   == "sing")
assoc.plur       <- subset(d.prop, related     == "assoc" & n2num   == "plur")
assoc.sing       <- subset(d.prop, related     == "assoc" & n2num   == "sing")
unrel.plur       <- subset(d.prop, related     == "unrel" & n2num   == "plur")
unrel.sing       <- subset(d.prop, related     == "unrel" & n2num   == "sing")

