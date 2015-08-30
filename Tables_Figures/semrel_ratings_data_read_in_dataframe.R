# -------------------- SEMREL: SET UP INPUT FILE(S) -----------------------------------
d.base           <- read.csv("data/allregdata.csv")
names(d.base)[names(d.base) == 'LengthSylHead'] <- 'LengthSyll.Head'
names(d.base)[names(d.base) == 'LogFr.Head'] <- 'LogFreq.Head'

# -------------------- SEMREL: MAIN SUBSETS  ---------
d.sr            <- subset(d.base, exp == "SemRel")
d.sr$back.trans <- ((sin(d.sr$AssArc.H.L/2))^2)
d.sr$integrated <- as.factor(d.sr$integrated)
d.sr$related    <- as.factor(d.sr$related)
d.sr$related    <- droplevels(d.sr$rel)
d.sr$n2num      <- as.factor(d.sr$n2num)
d.sr$item       <- as.factor(d.sr$item)
line             = rep(c("-"), times = 40, fill = 80)
br               = "\n"
# -------------------- SEMREL: ADDITIONAL SUBSETTING -----------------
integ <- subset(d.sr, integrated   ==  "integ")
unint <- subset(d.sr, integrated   ==  "unint")
relat <- subset(d.sr, related  ==  "rel")
unrel <- subset(d.sr, related  ==  "unrel")
sing  <- subset(d.sr, n2num    ==  "sing")
plur  <- subset(d.sr, n2num    ==  "plur")

#Below, additional subsetted groups
relat.int.plur   <- subset(d.sr, related     == "rel"   & integrated == "integ" & n2num == "plur")
relat.int.sing   <- subset(d.sr, related     == "rel"   & integrated == "integ" & n2num == "sing")
relat.unint.plur <- subset(d.sr, related     == "rel"   & integrated == "unint" & n2num == "plur")
relat.unint.sing <- subset(d.sr, related     == "rel"   & integrated == "unint" & n2num == "sing")
unrel.int.plur   <- subset(d.sr, related     == "unrel" & integrated == "integ" & n2num == "plur")
unrel.int.sing   <- subset(d.sr, related     == "unrel" & integrated == "integ" & n2num == "sing")
unrel.unint.plur <- subset(d.sr, related     == "unrel" & integrated == "unint" & n2num == "plur")
unrel.unint.sing <- subset(d.sr, related     == "unrel" & integrated == "unint" & n2num == "sing")
relat.plur       <- subset(d.sr, related     == "rel"   & n2num      == "plur")
relat.sing       <- subset(d.sr, related     == "rel"   & n2num      == "sing")
unrel.plur       <- subset(d.sr, related     == "unrel" & n2num      == "plur")
unrel.sing       <- subset(d.sr, related     == "unrel" & n2num      == "sing")
integ.plur       <- subset(d.sr, integrated  == "integ" & n2num      == "plur")
integ.sing       <- subset(d.sr, integrated  == "integ" & n2num      == "sing")
unint.plur       <- subset(d.sr, integrated  == "unint" & n2num      == "plur")
unint.sing       <- subset(d.sr, integrated  == "unint" & n2num      == "sing")
integ.relat      <- subset(d.sr, integrated  == "integ" & related    == "rel")
integ.unrel      <- subset(d.sr, integrated  == "integ" & related    == "unrel")
unint.relat      <- subset(d.sr, integrated  == "unint" & related    == "rel")
unint.unrel      <- subset(d.sr, integrated  == "unint" & related    == "unrel")

