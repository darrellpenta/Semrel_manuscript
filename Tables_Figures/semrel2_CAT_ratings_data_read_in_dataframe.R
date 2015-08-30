
# -------------------- CAT CORD: SET UP INPUT FILE(S) -----------------------------------
d.base           <- read.csv("data/allregdata.csv")
names(d.base)[names(d.base) == 'LengthSylHead'] <- 'LengthSyll.Head'
names(d.base)[names(d.base) == 'LogFr.Head'] <- 'LogFreq.Head'
# -------------------- CAT CORD: SUBSETS  ---------
d.cat            <- subset(d.base, exp == "Cat")
d.cat$integrated <- as.factor(d.cat$integrated)
d.cat$related    <- as.factor(d.cat$related)
d.cat$related    <- droplevels(d.cat$rel)
d.cat$n2num      <- as.factor(d.cat$n2num)
d.cat$item       <- as.factor(d.cat$item)
line             = rep(c("-"), times = 40, fill = 80)
br               = "\n"


# Below, designates various subsets of the original data file
integ <- subset(d.cat, integrated   ==  "integ")
unint <- subset(d.cat, integrated   ==  "unint")
relat <- subset(d.cat, related      ==  "rel")
unrel <- subset(d.cat, related      ==  "unrel")
sing  <- subset(d.cat, n2num        ==  "sing")
plur  <- subset(d.cat, n2num        ==  "plur")

#Below, additional subsetted groups
relat.int.plur   <- subset(d.cat, related     == "rel"   & integrated == "integ" & n2num == "plur")
relat.int.sing   <- subset(d.cat, related     == "rel"   & integrated == "integ" & n2num == "sing")
relat.unint.plur <- subset(d.cat, related     == "rel"   & integrated == "unint" & n2num == "plur")
relat.unint.sing <- subset(d.cat, related     == "rel"   & integrated == "unint" & n2num == "sing")
unrel.int.plur   <- subset(d.cat, related     == "unrel" & integrated == "integ" & n2num == "plur")
unrel.int.sing   <- subset(d.cat, related     == "unrel" & integrated == "integ" & n2num == "sing")
unrel.unint.plur <- subset(d.cat, related     == "unrel" & integrated == "unint" & n2num == "plur")
unrel.unint.sing <- subset(d.cat, related     == "unrel" & integrated == "unint" & n2num == "sing")
relat.plur       <- subset(d.cat, related     == "rel"   & n2num      == "plur")
relat.sing       <- subset(d.cat, related     == "rel"   & n2num      == "sing")
unrel.plur       <- subset(d.cat, related     == "unrel" & n2num      == "plur")
unrel.sing       <- subset(d.cat, related     == "unrel" & n2num      == "sing")
integ.plur       <- subset(d.cat, integrated  == "integ" & n2num      == "plur")
integ.sing       <- subset(d.cat, integrated  == "integ" & n2num      == "sing")
unint.plur       <- subset(d.cat, integrated  == "unint" & n2num      == "plur")
unint.sing       <- subset(d.cat, integrated  == "unint" & n2num      == "sing")
integ.relat      <- subset(d.cat, integrated  == "integ" & related    == "rel")
integ.unrel      <- subset(d.cat, integrated  == "integ" & related    == "unrel")
unint.relat      <- subset(d.cat, integrated  == "unint" & related    == "rel")
unint.unrel      <- subset(d.cat, integrated  == "unint" & related    == "unrel")