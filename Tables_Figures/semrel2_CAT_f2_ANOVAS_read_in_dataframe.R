

# ====================================================================================================
# CATEGORY COORDINATE ITEMS ANALYSES ------------------------------------------------
# ====================================================================================================
#
# PREPARE DATA FILE FOR ANALYSES -------------------------------------------------
#
f2errout <- read.table("data/SR2_F2_errcat.txt", header = T)
d <- f2errout
d$item <- as.factor(d$item)
d$pct <- ifelse(d$errd == 0 & d$errcord == 0, 0, (d$errd / (d$errcord)) * 100)
data.item <- aggregate(d$pct, list(d$item, d$related, d$n2num ), mean)
colnames(data.item) <- c("item", "related", "n2num", "error") 

relat       <- subset(data.item, related  == "rel")
unrel       <- subset(data.item, related  == "unrel")
sing        <- subset(data.item, n2num    == "sing")
plur        <- subset(data.item, n2num    == "plur")
relat.plur  <- subset(data.item, related  == "rel"   & n2num   == "plur")
relat.sing  <- subset(data.item, related  == "rel"   & n2num   == "sing")
unrel.plur  <- subset(data.item, related  == "unrel" & n2num   == "plur")
unrel.sing  <- subset(data.item, related  == "unrel" & n2num   == "sing")



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
  
  n = c(length( data.item$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),
  
  N = c(length( data.item$error),
        length( relat$error),
        length( unrel$error),
        length( plur$error),
        length( sing$error),
        length( relat.plur$error),
        length( relat.sing$error),
        length( unrel.plur$error),
        length( unrel.sing$error)),
  
  mean = c(mean( data.item$error),
           mean( relat$error),
           mean( unrel$error),
           mean( plur$error),
           mean( sing$error),
           mean( relat.plur$error),
           mean( relat.sing$error),
           mean( unrel.plur$error),
           mean( unrel.sing$error)),
  
  sd = c(sd( data.item$error),
         sd( relat$error),
         sd( unrel$error),
         sd( plur$error),
         sd( sing$error),
         sd( relat.plur$error),
         sd( relat.sing$error),
         sd( unrel.plur$error),
         sd( unrel.sing$error)),
  
  se = c(sd( data.item$error)  / sqrt( length( data.item$error)),
         sd( relat$error)      / sqrt( length( relat$error)),
         sd( unrel$error)      / sqrt( length( unrel$error)),
         sd( plur$error)       / sqrt( length( plur$error)),
         sd( sing$error)       / sqrt( length( sing$error)),
         sd( relat.plur$error) / sqrt( length( relat.plur$error) ),
         sd( relat.sing$error) / sqrt( length( relat.sing$error)),
         sd( unrel.plur$error) / sqrt( length( unrel.plur$error)),
         sd( unrel.sing$error) / sqrt( length( unrel.sing$error))
  ))

