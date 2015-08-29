rm(list = ls()) 
library(languageR) 
library(xlsx)
# library(xtable)

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


get_stars = function(p) {
  stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
  codes = c("***" , "**","*", ".", " ")
  codes[stars]
}
options(scipen=1)



# -------------------- TABLE 2 SEMREL: RELATEDNESS -----------------------------



aov.rel <- summary(aov(RelatedHL ~ related + Error(item / related ), data = d.sr))
p <- zapsmall(aov.rel[[2]][[1]][["Pr(>F)"]][1], digits=6)

exp1.related <- data.frame(
  
  Local.Noun.Number= c(
    "Singular",
    "Plural", 
    "Mean"               ),
  
  Related =            c(
    paste( round( mean( relat.sing$RelatedHL), 2), " (", round( sd( relat.sing$RelatedHL), 2), ")", sep = ""),
    paste( round( mean(relat.plur$RelatedHL), 2), " (", round( sd( relat.plur$RelatedHL), 2), ")", sep = ""),
    paste( round( mean(relat$RelatedHL), 2))
  ),
  
  Unrelated =           c(
    paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
    paste( round( mean(unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
    paste( round( mean(unrel$RelatedHL), 2))
  ),
  
  pval =                 c(
    paste(" ", sep = ""),
    paste(" ", sep = ""),
    paste( p,  get_stars(p))
  )
)


write.xlsx(exp1.related, file="output/manuscript_tables/table2_relatedness_ratings.xlsx", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

sink("output/manuscript_anovas/table2_relatedness_ratings.txt")
cat("Table 2: Experiment 1 Critical Item Mean Relatedness Ratings", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print(aov.rel)
sink()

# -------------------- TABLE 3 SEMREL: INTEGRATION ------------------------


aov.int.unint <- summary(aov(Integrated ~ integrated + Error(item / integrated ), data = d.sr))
p.int.unint <- zapsmall(aov.int.unint[[2]][[1]][["Pr(>F)"]][1], digits=6)

aov.int <- summary(aov(Integrated ~ related + Error(item / related ), data = integ))
p.int<- zapsmall(aov.int[[2]][[1]][["Pr(>F)"]][1], digits=6)

aov.unint <- summary(aov(Integrated ~ related + Error(item / related ), data = unint))
p.unint<- zapsmall(aov.unint[[2]][[1]][["Pr(>F)"]][1], digits=6)

exp1.integration <- data.frame(
  
  Semantic.Integration= c(
    "Integrated",
    "Integrated",
    "Integrated",
    "Unintegrated",
    "Unintegrated",
    "Unintegrated",
     paste( p.int.unint,  get_stars(p.int.unint))
    
                  ),
  
  Local.Noun.Number= c(
    "Singular",
    "Plural", 
    "Mean",
    "Singular",
    "Plural", 
    "Mean",
    paste(" ")
    ),
  
  
  
  Related =            c(
    paste( round( mean( relat.int.sing$Integrated), 2), " (", round( sd( relat.int.sing$Integrated), 2), ")", sep = ""),
    paste( round( mean(relat.int.plur$Integrated), 2), " (", round( sd( relat.int.plur$Integrated), 2), ")", sep = ""),
    paste( round( mean(integ.relat$Integrated), 2)),
    paste( round( mean( relat.unint.sing$Integrated), 2), " (", round( sd( relat.unint.sing$Integrated), 2), ")", sep = ""),
    paste( round( mean(relat.unint.plur$Integrated), 2), " (", round( sd( relat.unint.plur$Integrated), 2), ")", sep = ""),
    paste( round( mean(unint.relat$Integrated), 2)),
    paste(" ")    ),
                  
   Unrelated =            c(
     paste( round( mean( unrel.int.sing$Integrated), 2), " (", round( sd( unrel.int.sing$Integrated), 2), ")", sep = ""),
     paste( round( mean(unrel.int.plur$Integrated), 2), " (", round( sd( unrel.int.plur$Integrated), 2), ")", sep = ""),
     paste( round( mean(unint.relat$Integrated), 2)),
     paste( round( mean( unrel.unint.sing$Integrated), 2), " (", round( sd( unrel.unint.sing$Integrated), 2), ")", sep = ""),
     paste( round( mean(unrel.unint.plur$Integrated), 2), " (", round( sd( unrel.unint.plur$Integrated), 2), ")", sep = ""),
     paste( round( mean(unint.unrel$Integrated), 2)),
     paste(" ")             ),
  

  pval =                 c(
    paste(" ", sep = ""),
    paste(" ", sep = ""),
    paste( p.int,  get_stars(p.int)),
    paste(" ", sep = ""),
    paste(" ", sep = ""),
    paste( p.unint,  get_stars(p.unint)),
    paste(" ")

  )
)



write.xlsx(exp1.integration, file="output/manuscript_tables/table3_integration_ratings.xlsx", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

sink("output/manuscript_anovas/table3_integration_ratings.txt")
cat("Table 3: Experiment 1 Critical Item Mean Inteagration Ratings", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print(aov.int.unint)
print(aov.int)
print(aov.unint)

sink()

# -------------------- TABLE 4 SEMREL: ASSOCIATION ------------------



aov.assoc <- summary(aov(back.trans ~ related + Error(item / related ), data = d.sr))
p <- zapsmall(aov.assoc[[2]][[1]][["Pr(>F)"]][1], digits=6)

exp1.assoc <- data.frame(
  Local.Noun.Number= c(
    "Singular",
    "Plural"
    ),
  
  Related =            c(
    paste( round( mean( relat.sing$back.trans), 2), " (", round( sd( relat.sing$back.trans), 2), ")", sep = ""),
    paste( round( mean(relat.plur$back.trans), 2), " (", round( sd( relat.plur$back.trans), 2), ")", sep = "")
  ),
  
  Unrelated =           c(
    paste( round( mean( unrel.sing$back.trans), 2), " (", round( sd( unrel.sing$back.trans), 2), ")", sep = ""),
    paste( round( mean(unrel.plur$back.trans), 2), " (", round( sd( unrel.plur$back.trans), 2), ")", sep = "")
  ),
  
  pval =                 c(
    paste(" ", sep = ""),
    paste( p,  get_stars(p))
  )
)


write.xlsx(exp1.related, file="output/manuscript_tables/table4_association_proportions.xlsx", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

sink("output/manuscript_anovas/table3_association_proportions.txt")
cat("Table 4: Experiment 1 Critical Item Mean Head-to-Local Association Proportions", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print(aov.assoc)
sink()


# --------------------TABLE 5 SEMREL: ERROR RATES AND RESPONSE COUNTS ------------


exp1.err.rates <- data.frame(
  
  Condition= c(
    "ReLated.Integrated",
    "ReLated.Integrated",
    "ReLated.Unintegrated",
    "ReLated.Unintegrated",
    "UnreLated.Integrated",
    "UnreLated.Integrated",
    "UnreLated.Unintegrated",
    "UnreLated.Unintegrated"
  ),
  
  Local.Noun.Number = c(
    "Plural",
    "Singular",    
    "Plural",
    "Singular",    
    "Plural",
    "Singular",    
    "Plural",
    "Singular"   
  ),
  
  Error.Rate   = c(
100*(sum(relat.int.plur$errd)   / sum(relat.int.plur$errcord)) ,
100*(sum(relat.int.sing$errd)   / sum(relat.int.sing$errcord)) ,
100*(sum(relat.unint.plur$errd) / sum(relat.unint.plur$errcord)),
100*(sum(relat.unint.sing$errd) / sum(relat.unint.sing$errcord)),
100*(sum(unrel.int.plur$errd)   / sum(unrel.int.plur$errcord)),
100*(sum(unrel.int.sing$errd)   / sum(unrel.int.sing$errcord)),
100*(sum(unrel.unint.plur$errd) / sum(unrel.unint.plur$errcord)),
100*(sum(unrel.unint.sing$errd) / sum(unrel.unint.sing$errcord))
  ))    
    
    
  

write.xlsx(exp1.related, file="output/manuscript_tables/table5_errorrates_responsecountes.xlsx", 
           col.names=TRUE, row.names=TRUE, append=FALSE)







# 
# # -------------------- SEMREL: SET UP OUTPUT FILE
# sink("output/norming/SEMREL norming.txt")
# cat("SEMREL NORMING ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
# 
# 
# # -------------------- SEMREL: NUISANCE FACTORS
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** NUISANCE FACTORS **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- SEMREL: PREPOSITION -------------------------------------------------------
# cat( br, rep(c("="), times = 50, quote = F), br)
# cat("PREPOSITION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.freq <- summary(aov(LogFreq.Prep ~ integrated + Error(item / integrated ), data = d.sr))
# p <- zapsmall(aov.freq[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Preposition  = c("Integrated","Unintegrated","PVal")
# 
# prep.freq = data.frame(
#   c(
#     paste( round( mean( integ$LogFreq.Prep), 2), " (", round( sd( integ$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( mean( unint$LogFreq.Prep), 2), " (", round( sd( unint$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(prep.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(Preposition, prep.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lc <- summary(aov(LengthChar.Prep ~ integrated + Error(item / integrated ), data = d.sr))
# p <- zapsmall(aov.lc[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( integ$LengthChar.Prep), 2), " (", round( sd( integ$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( mean( unint$LengthChar.Prep), 2), " (", round( sd( unint$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.ph <- summary(aov(LengthPhon.Prep ~ integrated + Error(item / integrated), data = d.sr))
# p <- zapsmall(aov.ph[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( integ$LengthPhon.Prep), 2), " (", round( sd( integ$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( mean( unint$LengthPhon.Prep), 2), " (", round( sd( unint$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.syl <- summary(aov(LengthSyll.Prep ~ integrated + Error(item / integrated), data = d.sr))
# p <- zapsmall(aov.syl[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( integ$LengthSyll.Prep), 2), " (", round( sd( integ$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( mean( unint$LengthSyll.Prep), 2), " (", round( sd( unint$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syl. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- SEMREL: ADJECTIVE -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ADJECTIVE", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.fra <- summary(aov(LogFreq.Adj ~ related + Error(item / related), data = d.sr))
# p <- zapsmall(aov.fra[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Adjective  = c("Related","Unrelated", "PVal")
# adj.freq = data.frame(
#   c(
#     paste( round( mean( relat$LogFreq.Adj), 2), " (", round( sd( relat$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Adj), 2), " (", round( sd( unrel$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(adj.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(Adjective, adj.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.cha <- summary(aov(LengthChar.Adj ~ related + Error(item / related), data = d.sr))
# p <- zapsmall(aov.cha[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthChar.Adj), 2), " (", round( sd( relat$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Adj), 2), " (", round( sd( unrel$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.pha <- summary(aov(LengthPhon.Adj ~ related + Error(item / related ), data = d.sr))
# p <- zapsmall(aov.pha[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthPhon.Adj), 2), " (", round( sd( relat$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Adj), 2), " (", round( sd( unrel$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.sya <- summary(aov(LengthSyll.Adj ~ related + Error(item / related ), data = d.sr))
# p <- zapsmall(aov.sya[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthSyll.Adj), 2), " (", round( sd( relat$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Adj), 2), " (", round( sd( unrel$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- SEMREL: RELATED LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED LOCAL NOUN SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frsp <- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.frsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# RelatedLN  = c("Singular","Plural", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( relat.sing$LogFreq.N1), 2), " (", round( sd( relat.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LogFreq.N1), 2), " (", round( sd( relat.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(RelatedLN, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcsp <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.lcsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthChar.Noun), 2), " (", round( sd( relat.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LengthChar.Noun), 2), " (", round( sd( relat.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpsp <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.lpsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthPhon.Noun), 2), " (", round( sd( relat.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LengthPhon.Noun), 2), " (", round( sd( relat.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthSyll.Noun), 2), " (", round( sd( relat.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LengthSyll.Noun), 2), " (", round( sd( relat.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- SEMREL: UNRELATED LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED LOCAL NOUN SING. vs. PLUR.", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frsp <- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.frsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# UnrelatedLN  = c("Singular","Plural", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LogFreq.N1), 2), " (", round( sd( unrel.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LogFreq.N1), 2), " (", round( sd( unrel.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(UnrelatedLN, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcsp <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.lcsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthChar.Noun), 2), " (", round( sd( unrel.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthChar.Noun), 2), " (", round( sd( unrel.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpsp <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.lpsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthPhon.Noun), 2), " (", round( sd( unrel.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthPhon.Noun), 2), " (", round( sd( unrel.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthSyll.Noun), 2), " (", round( sd( unrel.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthSyll.Noun), 2), " (", round( sd( unrel.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- SEMREL: RELATED vs UNRELATED SINGULAR LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs UNRELATED SINGULAR LOCAL NOUN", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frss <- summary(aov(LogFreq.N1 ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.frss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# LocalNoun  = c("Rel.Sing","Unrel.Sing", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( relat.sing$LogFreq.N1), 2), " (", round( sd( relat.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LogFreq.N1), 2), " (", round( sd( unrel.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(LocalNoun, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcss <- summary(aov(LengthChar.Noun ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.lcss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthChar.Noun), 2), " (", round( sd( relat.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LengthChar.Noun), 2), " (", round( sd( unrel.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpss <- summary(aov(LengthPhon.Noun ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.lpss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthPhon.Noun), 2), " (", round( sd( relat.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LengthPhon.Noun), 2), " (", round( sd( unrel.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthSyll.Noun), 2), " (", round( sd( relat.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LengthSyll.Noun), 2), " (", round( sd( unrel.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- SEMREL: RELATED vs UNRELATED PLURAL LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs UNRELATED PLURAL LOCAL NOUN", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frss <- summary(aov(LogFreq.N1 ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.frss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# LocalNoun  = c("Rel.Plur","Unrel.Plur", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( relat.plur$LogFreq.N1), 2), " (", round( sd( relat.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LogFreq.N1), 2), " (", round( sd( unrel.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(LocalNoun, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcss <- summary(aov(LengthChar.Noun ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.lcss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat.plur$LengthChar.Noun), 2), " (", round( sd( relat.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthChar.Noun), 2), " (", round( sd( unrel.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpss <- summary(aov(LengthPhon.Noun ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.lpss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat.plur$LengthPhon.Noun), 2), " (", round( sd( relat.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthPhon.Noun), 2), " (", round( sd( unrel.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat.plur$LengthSyll.Noun), 2), " (", round( sd( relat.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthSyll.Noun), 2), " (", round( sd( unrel.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- SEMREL: 2 x 2 LOCAL NOUN ANOVAs -------------------------------------------------------
# # ----------------------------- ::FREQUENCY------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA LOG FREQUENCY ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.fr22 <- summary(aov(LogFreq.N1 ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.sr))
# p.rel <- zapsmall(aov.fr22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.freq.rel = data.frame(
#   c(
#     paste( round( mean( relat$LogFreq.N1), 2), " (", round( sd( relat$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.N1), 2), " (", round( sd( unrel$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.freq.rel)[1] <- "Frequency" 
# ds.rel <- cbind.data.frame(Condition, noun.freq.rel)
# 
# 
# p.num <- zapsmall(aov.fr22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.freq.num = data.frame(
#   c(
#     paste( round( mean( sing$LogFreq.N1), 2), " (", round( sd( sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( plur$LogFreq.N1), 2), " (", round( sd( plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.freq.num)[1] <- "Frequency" 
# ds.num <- cbind.data.frame(Condition, noun.freq.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# # ----------------------------- ::CHAR.LENGTH------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA CHARACTER LENGTH ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.cl22 <- summary(aov(LengthChar.Noun ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.sr))
# p.rel <- zapsmall(aov.cl22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.char.rel = data.frame(
#   c(
#     paste( round( mean( relat$LengthChar.Noun), 2), " (", round( sd( relat$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Noun), 2), " (", round( sd( unrel$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.char.rel)[1] <- "Char. Length" 
# ds.rel <- cbind.data.frame(Condition, noun.char.rel)
# 
# 
# p.num <- zapsmall(aov.cl22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.char.num = data.frame(
#   c(
#     paste( round( mean( sing$LengthChar.Noun), 2), " (", round( sd( sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( plur$LengthChar.Noun), 2), " (", round( sd( plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.char.num)[1] <- "Char. Length" 
# ds.num <- cbind.data.frame(Condition, noun.char.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# # ----------------------------- ::PHON.LENGTH------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA PHONEME LENGTH ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.pl22 <- summary(aov(LengthPhon.Noun ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.sr))
# p.rel <- zapsmall(aov.pl22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.phon.rel = data.frame(
#   c(
#     paste( round( mean( relat$LengthPhon.Noun), 2), " (", round( sd( relat$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Noun), 2), " (", round( sd( unrel$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.phon.rel)[1] <- "Phon. Length" 
# ds.rel <- cbind.data.frame(Condition, noun.phon.rel)
# 
# 
# p.num <- zapsmall(aov.pl22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.phon.num = data.frame(
#   c(
#     paste( round( mean( sing$LengthPhon.Noun), 2), " (", round( sd( sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( plur$LengthPhon.Noun), 2), " (", round( sd( plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.phon.num)[1] <- "Phon. Length" 
# ds.num <- cbind.data.frame(Condition, noun.phon.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# # ----------------------------- ::SYLL.LENGTH------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA SYLLABLE LENGTH ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.sl22 <- summary(aov(LengthSyll.Noun ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.sr))
# p.rel <- zapsmall(aov.sl22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.phon.rel = data.frame(
#   c(
#     paste( round( mean( relat$LengthSyll.Noun), 2), " (", round( sd( relat$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Noun), 2), " (", round( sd( unrel$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.phon.rel)[1] <- "Syll. Length" 
# ds.rel <- cbind.data.frame(Condition, noun.phon.rel)
# 
# 
# p.num <- zapsmall(aov.sl22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.phon.num = data.frame(
#   c(
#     paste( round( mean( sing$LengthSyll.Noun), 2), " (", round( sd( sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( plur$LengthSyll.Noun), 2), " (", round( sd( plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.phon.num)[1] <- "Syll. Length" 
# ds.num <- cbind.data.frame(Condition, noun.phon.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# 
# # -------------------- SEMREL:  2 x 2 ANOVA RELATEDNESS -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 ANOVA RELATEDNESS", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# rel22 <- summary(aov(RelatedHL ~ related * n2num + Error(item / related * n2num), data = d.sr))
# print(rel22)
# # -------------------- SEMREL: RELATEDNESS RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.rel <- summary(aov(RelatedHL ~ related + Error(item / related), data = d.sr))
# p <- zapsmall(aov.rel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Mean Related", "Mean Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$RelatedHL), 2), " (", round( sd( relat$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel$RelatedHL), 2), " (", round( sd( unrel$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "Relatedness" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- SEMREL: RELATEDNESS SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$RelatedHL), 2), " (", round( sd( relat.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$RelatedHL), 2), " (", round( sd( relat.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Related" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Unrelated" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- SEMREL: RELATEDNESS SING vs. UNRELATEDNESS SING -------------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(RelatedHL ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Related.Sing","Unrelated.Sing", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$RelatedHL), 2), " (", round( sd( relat.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Total" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # -------------------- SEMREL: RELATEDNESS PLUR vs. UNRELATEDNESS PLUR -------------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(RelatedHL ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( relat.plur$RelatedHL), 2), " (", round( sd( relat.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Total" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# 
# # -------------------- SEMREL: INTEGRATION NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** SEMANTIC INTEGRATION **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- SEMREL:  2 x 2 x 2 ANOVA INTEGRATION -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 x 2 ANOVA INTEGRATION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# int222 <- summary(aov(Integrated ~ integrated * related * n2num + Error(item / integrated * related * n2num), data = d.sr))
# print(int222)
# # -------------------- SEMREL: INTEGRATED vs. UNINTEGRATED -------------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED vs. UNINTEGRATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunint <- summary(aov(Integrated ~ integrated + Error(item / integrated), data = d.sr))
# p <- zapsmall(aov.intunint[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integrated  = c("Integrated","Unintegrated", "PVal")
# intunint = data.frame(
#   c(
#     paste( round( mean( integ$Integrated), 2), " (", round( sd( integ$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unint$Integrated), 2), " (", round( sd( unint$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunint)[1] <- "Total" 
# intunint.means <- cbind.data.frame(Integrated, intunint)
# 
# print(intunint.means)
# # -------------------- SEMREL: RELATED vs. UNRELATED -------------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunint <- summary(aov(Integrated ~ related + Error(item / related), data = d.sr))
# p <- zapsmall(aov.intunint[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$Integrated), 2), " (", round( sd( relat$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel$Integrated), 2), " (", round( sd( unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "Total" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- SEMREL: INTEGRATED RELATED vs. UNITEGRATED RELATED ------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Related vs. UNINTEGRATED Related", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relrel <- summary(aov(Integrated ~ integrated + Error(item / integrated), data = relat))
# p <- zapsmall(aov.relrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integrated  = c("Integrated.Rel","Unintegrated.Rel", "PVal")
# relrel = data.frame(
#   c(
#     paste( round( mean( integ.relat$Integrated), 2), " (", round( sd( integ.relat$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unint.relat$Integrated), 2), " (", round( sd( unint.relat$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relrel)[1] <- "Total" 
# relrel.means <- cbind.data.frame(Integrated, relrel)
# 
# print(relrel.means)
# # -------------------- SEMREL: INTEGRATED UNRELATED vs. UNITEGRATED UNRELATED ------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Unrelated vs. UNINTEGRATED Unrelated", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unrelunrel <- summary(aov(Integrated ~ integrated + Error(item / integrated), data = unrel))
# p <- zapsmall(aov.unrelunrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integrated  = c("Integrated.Unrel","Unintegrated.Unrel", "PVal")
# unrelunrel = data.frame(
#   c(
#     paste( round( mean( integ.unrel$Integrated), 2), " (", round( sd( integ.unrel$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unint.unrel$Integrated), 2), " (", round( sd( unint.unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelunrel)[1] <- "Total" 
# unrelunrel.means <- cbind.data.frame(Integrated, unrelunrel)
# 
# print(unrelunrel.means)
# # -------------------- SEMREL: INTEGRATED Related vs. Unrelated -------------------------------------------------------
# # ----------------------------- ::INT RELATED vs UNRELATED------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Related vs. Unrelated", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intrel <- summary(aov(Integrated ~ related + Error(item / related), data = integ))
# p <- zapsmall(aov.intrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# intrel = data.frame(
#   c(
#     paste( round( mean( integ.relat$Integrated), 2), " (", round( sd( integ.relat$Integrated), 2), ")", sep = ""),
#     paste( round( mean( integ.unrel$Integrated), 2), " (", round( sd( integ.unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intrel)[1] <- "Relat" 
# intrel.means <- cbind.data.frame(Relatedness, intrel)
# 
# print(intrel.means)
# # ----------------------------- ::UNINT RELATED vs UNRELATED------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNINTEGRATED Related vs. Unrelated", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unintrel <- summary(aov(Integrated ~ related + Error(item / related), data = unint))
# p <- zapsmall(aov.unintrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# unintrel = data.frame(
#   c(
#     paste( round( mean( unint.relat$Integrated), 2), " (", round( sd( unint.relat$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unint.unrel$Integrated), 2), " (", round( sd( unint.unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintrel)[1] <- "Relat" 
# unintrel.means <- cbind.data.frame(Relatedness, unintrel)
# 
# print(unintrel.means)
# # -------------------- SEMREL: INTEGRATED SING vs. UNINTEGRATED SING -------------------------------------------------------
# # ----------------------------- ::INT. SING vs UNINT. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED SING vs. UNINTEGRATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Int.Sing","Unint.Sing", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( integ.sing$RelatedHL), 2), " (", round( sd( integ.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unint.sing$RelatedHL), 2), " (", round( sd( unint.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Total" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # -------------------- SEMREL: INTEGRATION PLUR vs. UNINTEGRATED PLUR -------------------------------------------------------
# # ----------------------------- ::INT. PLUR vs UNINT. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED PLUR vs. UNINTEGRATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( integ.plur$RelatedHL), 2), " (", round( sd( integ.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unint.plur$RelatedHL), 2), " (", round( sd( unint.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Total" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- SEMREL: INTEGRATED Related SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Related SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intrelsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = integ.relat))
# p <- zapsmall(aov.intrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intrelsp = data.frame(
#   c(
#     paste( round( mean( relat.int.sing$Integrated), 2), " (", round( sd( relat.int.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( relat.int.plur$Integrated), 2), " (", round( sd( relat.int.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intrelsp)[1] <- "Integrated.Related" 
# intrelsp.means <- cbind.data.frame(N2Num, intrelsp)
# 
# print(intrelsp.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Unrelated SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.intunrelsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = integ.unrel))
# p <- zapsmall(aov.intunrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intunrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.int.sing$Integrated), 2), " (", round( sd( unrel.int.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.int.plur$Integrated), 2), " (", round( sd( unrel.int.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunrelsp)[1] <- "Integrated.Unrelated" 
# intunrelsp.means <- cbind.data.frame(N2Num, intunrelsp)
# 
# print(intunrelsp.means)
# # -------------------- SEMREL: INTEGRATED Related SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Related SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intrelsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = integ.relat))
# p <- zapsmall(aov.intrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intrelsp = data.frame(
#   c(
#     paste( round( mean( relat.int.sing$Integrated), 2), " (", round( sd( relat.int.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( relat.int.plur$Integrated), 2), " (", round( sd( relat.int.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intrelsp)[1] <- "Integrated.Related" 
# intrelsp.means <- cbind.data.frame(N2Num, intrelsp)
# 
# print(intrelsp.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Unrelated SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.intunrelsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = integ.unrel))
# p <- zapsmall(aov.intunrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intunrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.int.sing$Integrated), 2), " (", round( sd( unrel.int.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.int.plur$Integrated), 2), " (", round( sd( unrel.int.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunrelsp)[1] <- "Integrated.Unrelated" 
# intunrelsp.means <- cbind.data.frame(N2Num, intunrelsp)
# 
# print(intunrelsp.means)
# # -------------------- SEMREL: UNINTEGRATED Related SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNINTEGRATED Related SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unintrelsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = unint.relat))
# p <- zapsmall(aov.unintrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unintrelsp = data.frame(
#   c(
#     paste( round( mean( relat.unint.sing$Integrated), 2), " (", round( sd( relat.unint.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( relat.unint.plur$Integrated), 2), " (", round( sd( relat.unint.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintrelsp)[1] <- "Unintegrated.Related" 
# unintrelsp.means <- cbind.data.frame(N2Num, unintrelsp)
# 
# print(unintrelsp.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED Unrelated SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unintunrelsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = unint.unrel))
# p <- zapsmall(aov.unintunrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unintunrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.unint.sing$Integrated), 2), " (", round( sd( unrel.unint.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.unint.plur$Integrated), 2), " (", round( sd( unrel.unint.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintunrelsp)[1] <- "Integrated.Unrelated" 
# unintunrelsp.means <- cbind.data.frame(N2Num, unintunrelsp)
# 
# print(unintunrelsp.means)
# # -------------------- SEMREL: INTEGRATED SING/PLUR vs. UNINTEGRATED SING/PLUR -----------------------------------------------
# # ----------------------------- ::INTEGRATED SING vs UNINTEGRATED SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED SING vs. UNINTEGRATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.sings <- summary(aov(Integrated ~ integrated + Error(item / integrated), data = sing))
# p <- zapsmall(aov.sings[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integ  = c("Integ.Sing","Unint.Sing", "PVal")
# intunintss = data.frame(
#   c(
#     paste( round( mean( integ.sing$Integrated), 2), " (", round( sd( integ.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unint.sing$Integrated), 2), " (", round( sd( unint.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintss)[1] <- "Total" 
# intunintss.means <- cbind.data.frame(Integ, intunintss)
# 
# print(intunintss.means)
# # ----------------------------- ::INTEGRATED PLUR vs UNINTEGRATED PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED PLUR vs. UNINTEGRATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.plurs <- summary(aov(Integrated ~ integrated + Error(item / integrated), data = plur))
# p <- zapsmall(aov.plurs[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integ  = c("Integ.Plur","Unint.Plur", "PVal")
# intunintpp = data.frame(
#   c(
#     paste( round( mean( integ.plur$Integrated), 2), " (", round( sd( integ.plur$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unint.plur$Integrated), 2), " (", round( sd( unint.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintpp)[1] <- "Total" 
# intunintpp.means <- cbind.data.frame(Integ, intunintpp)
# 
# print(intunintpp.means)
# 
# # -------------------- SEMREL: ASSOCIATION NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** ASSOCIATION **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- SEMREL:  2 x 2 ANOVA ASSOCIATION -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 ANOVA ASSOCIATION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# ass22 <- summary(aov(AssArc.H.L ~ related * n2num + Error(item / related * n2num), data = d.sr))
# print(ass22)
# # -------------------- SEMREL: ASSOCIATED RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION: RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.ass <- summary(aov(AssArc.H.L ~ related + Error(item / related), data = d.sr))
# p <- zapsmall(aov.ass[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Mean Related", "Mean Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$AssArc.H.L), 2), " (", round( sd( relat$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel$AssArc.H.L), 2), " (", round( sd( unrel$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "AssArc HL" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- SEMREL: ASSOCIATED SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION: RELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$AssArc.H.L), 2), " (", round( sd( relat.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$AssArc.H.L), 2), " (", round( sd( relat.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "AssArc HL" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION: UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$AssArc.H.L), 2), " (", round( sd( unrel.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$AssArc.H.L), 2), " (", round( sd( unrel.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "AssArc HL" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- SEMREL: ASSOCIATED: REL SING vs. ASSOCIATED: UNREL SING -----------------------------------------------
# # ----------------------------- ::ASS.REL. SING vs ASS. UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATED RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(AssArc.H.L ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Related.Sing","Unrelated.Sing", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$AssArc.H.L), 2), " (", round( sd( relat.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$AssArc.H.L), 2), " (", round( sd( unrel.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Total" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # -------------------- SEMREL: ASSOCIATED: REL. PLUR vs. ASSOCIATED: REL PLUR ------------------------------------------------
# # ----------------------------- ::ASS.REL. PLUR vs ASS.UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(AssArc.H.L ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( relat.plur$AssArc.H.L), 2), " (", round( sd( relat.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$AssArc.H.L), 2), " (", round( sd( unrel.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Total" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- SEMREL: PLAUSIBILITY NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** PLAUSIBILITY  **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- SEMREL:  2 x 2 x 2 ANOVA PLAUSIBILITY -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 x 2 ANOVA PLAUSIBILITY", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# int222 <- summary(aov(Plausibility ~ integrated * related * n2num + Error(item / integrated * related * n2num), data = d.sr))
# print(int222)
# 
# # -------------------- SEMREL: PLAUS: INTEGRATED vs. UNINTEGRATED -------------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED vs. UNINTEGRATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunint <- summary(aov(Plausibility ~ integrated + Error(item / integrated), data = d.sr))
# p <- zapsmall(aov.intunint[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Plausibility  = c("Integrated","Unintegrated", "PVal")
# intunint = data.frame(
#   c(
#     paste( round( mean( integ$Plausibility), 2), " (", round( sd( integ$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unint$Plausibility), 2), " (", round( sd( unint$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunint)[1] <- "Plaus.Total" 
# intunint.means <- cbind.data.frame(Plausibility, intunint)
# 
# print(intunint.means)
# # -------------------- SEMREL: PLAUS: RELATED vs. UNRELATED -------------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunint <- summary(aov(Plausibility ~ related + Error(item / related), data = d.sr))
# p <- zapsmall(aov.intunint[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$Plausibility), 2), " (", round( sd( relat$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel$Plausibility), 2), " (", round( sd( unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "Plaus.Total" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- SEMREL: PLAUS: INTEGRATED RELATED vs. UNITEGRATED RELATED ------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED Related vs. UNINTEGRATED Related", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relrel <- summary(aov(Plausibility ~ integrated + Error(item / integrated), data = relat))
# p <- zapsmall(aov.relrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Plausibility  = c("Integrated.Rel","Unintegrated.Rel", "PVal")
# relrel = data.frame(
#   c(
#     paste( round( mean( integ.relat$Plausibility), 2), " (", round( sd( integ.relat$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unint.relat$Plausibility), 2), " (", round( sd( unint.relat$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relrel)[1] <- "Plaus. Total" 
# relrel.means <- cbind.data.frame(Plausibility, relrel)
# 
# print(relrel.means)
# # -------------------- SEMREL: PLAUS: INTEGRATED UNRELATED vs. UNITEGRATED UNRELATED ------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED Unrelated vs. UNINTEGRATED Unrelated", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unrelunrel <- summary(aov(Plausibility ~ integrated + Error(item / integrated), data = unrel))
# p <- zapsmall(aov.unrelunrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Plausibility  = c("Integrated.Unrel","Unintegrated.Unrel", "PVal")
# unrelunrel = data.frame(
#   c(
#     paste( round( mean( integ.unrel$Plausibility), 2), " (", round( sd( integ.unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unint.unrel$Plausibility), 2), " (", round( sd( unint.unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelunrel)[1] <- "Plaus: Total" 
# unrelunrel.means <- cbind.data.frame(Plausibility, unrelunrel)
# 
# print(unrelunrel.means)
# # -------------------- SEMREL: PLAUS: INTEGRATED Related vs. Unrelated -------------------------------------------------------
# # ----------------------------- ::INT RELATED vs UNRELATED------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED Related vs. Unrelated", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intrel <- summary(aov(Plausibility ~ related + Error(item / related), data = integ))
# p <- zapsmall(aov.intrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# intrel = data.frame(
#   c(
#     paste( round( mean( integ.relat$Plausibility), 2), " (", round( sd( integ.relat$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( integ.unrel$Plausibility), 2), " (", round( sd( integ.unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intrel)[1] <- "Plaus. Total" 
# intrel.means <- cbind.data.frame(Relatedness, intrel)
# 
# print(intrel.means)
# # ----------------------------- ::UNINT RELATED vs UNRELATED------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat(" PLAUS: UNINTEGRATED Related vs. Unrelated", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unintrel <- summary(aov(Plausibility ~ related + Error(item / related), data = unint))
# p <- zapsmall(aov.unintrel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# unintrel = data.frame(
#   c(
#     paste( round( mean( unint.relat$Plausibility), 2), " (", round( sd( unint.relat$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unint.unrel$Plausibility), 2), " (", round( sd( unint.unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintrel)[1] <- "Plaus. Total" 
# unintrel.means <- cbind.data.frame(Relatedness, unintrel)
# 
# print(unintrel.means)
# # -------------------- SEMREL: PLAUS: INTEGRATED Related SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED Related SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intrelsp <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = integ.relat))
# p <- zapsmall(aov.intrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intrelsp = data.frame(
#   c(
#     paste( round( mean( relat.int.sing$Plausibility), 2), " (", round( sd( relat.int.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( relat.int.plur$Plausibility), 2), " (", round( sd( relat.int.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intrelsp)[1] <- "Plaus. Total" 
# intrelsp.means <- cbind.data.frame(N2Num, intrelsp)
# 
# print(intrelsp.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED Unrelated SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.intunrelsp <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = integ.unrel))
# p <- zapsmall(aov.intunrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intunrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.int.sing$Plausibility), 2), " (", round( sd( unrel.int.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.int.plur$Plausibility), 2), " (", round( sd( unrel.int.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunrelsp)[1] <- "Plaus. Total" 
# intunrelsp.means <- cbind.data.frame(N2Num, intunrelsp)
# 
# print(intunrelsp.means)
# # -------------------- SEMREL: PLAUS: UNINTEGRATED Related SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: UNINTEGRATED Related SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unintrelsp <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = unint.relat))
# p <- zapsmall(aov.unintrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unintrelsp = data.frame(
#   c(
#     paste( round( mean( relat.unint.sing$Plausibility), 2), " (", round( sd( relat.unint.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( relat.unint.plur$Plausibility), 2), " (", round( sd( relat.unint.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintrelsp)[1] <- "PLAUS. TOTAL" 
# unintrelsp.means <- cbind.data.frame(N2Num, unintrelsp)
# 
# print(unintrelsp.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED Unrelated SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unintunrelsp <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = unint.unrel))
# p <- zapsmall(aov.unintunrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unintunrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.unint.sing$Plausibility), 2), " (", round( sd( unrel.unint.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.unint.plur$Plausibility), 2), " (", round( sd( unrel.unint.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintunrelsp)[1] <- "Plaus. Total" 
# unintunrelsp.means <- cbind.data.frame(N2Num, unintunrelsp)
# 
# print(unintunrelsp.means)
# # -------------------- SEMREL: PLAUS:INTEGRATED SING/PLUR vs. UNINTEGRATED SING/PLUR -----------------------------------------
# # ----------------------------- ::INTEGRATED SING vs UNINTEGRATED SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED SING vs. UNINTEGRATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.sings <- summary(aov(Plausibility ~ integrated + Error(item / integrated), data = sing))
# p <- zapsmall(aov.sings[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integ  = c("Integ.Sing","Unint.Sing", "PVal")
# intunintss = data.frame(
#   c(
#     paste( round( mean( integ.sing$Plausibility), 2), " (", round( sd( integ.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unint.sing$Plausibility), 2), " (", round( sd( unint.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintss)[1] <- "Plaus. Total" 
# intunintss.means <- cbind.data.frame(Integ, intunintss)
# 
# print(intunintss.means)
# # ----------------------------- ::INTEGRATED PLUR vs UNINTEGRATED PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED PLUR vs. UNINTEGRATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.plurs <- summary(aov(Plausibility ~ integrated + Error(item / integrated), data = plur))
# p <- zapsmall(aov.plurs[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Integ  = c("Integ.Plur","Unint.Plur", "PVal")
# intunintpp = data.frame(
#   c(
#     paste( round( mean( integ.plur$Plausibility), 2), " (", round( sd( integ.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unint.plur$Plausibility), 2), " (", round( sd( unint.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintpp)[1] <- "Plaus. Total" 
# intunintpp.means <- cbind.data.frame(Integ, intunintpp)
# 
# print(intunintpp.means)
# # -------------------- SEMREL: PLAUS:RELATED SING/PLUR vs. UNRELATED SING/PLUR -----------------------------------------
# # ----------------------------- ::RELATED SING vs UNRELATED SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.sings <- summary(aov(Plausibility ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.sings[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Rel.Sing","Unrel..Sing", "PVal")
# relunrelss = data.frame(
#   c(
#     paste( round( mean( relat.sing$Plausibility), 2), " (", round( sd( relat.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Plausibility), 2), " (", round( sd( unrel.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrelss)[1] <- "Plaus. Total" 
# relunrelss.means <- cbind.data.frame(Relatedness, relunrelss)
# 
# print(relunrelss.means)
# # ----------------------------- ::RELATED PLUR vs UNRELATED PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: INTEGRATED PLUR vs. UNINTEGRATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.plurs <- summary(aov(Plausibility ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.plurs[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Rel.","Unrel.Plur", "PVal")
# relunrelpp = data.frame(
#   c(
#     paste( round( mean( relat.plur$Plausibility), 2), " (", round( sd( relat.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Plausibility), 2), " (", round( sd( unrel.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrelpp)[1] <- "Plaus. Total" 
# relunrelpp.means <- cbind.data.frame(Relatedness, relunrelpp)
# 
# print(relunrelpp.means)
# 
# sink()
# # ===============================================================================================
# # ===============================================================================================
# rm(list = ls()) 
# # -------------------- CAT CORD: SET UP INPUT FILE(S) -----------------------------------
# d.base           <- read.csv("data/allregdata.csv")
# names(d.base)[names(d.base) == 'LengthSylHead'] <- 'LengthSyll.Head'
# names(d.base)[names(d.base) == 'LogFr.Head'] <- 'LogFreq.Head'
# # -------------------- CAT CORD: SUBSETS  ---------
# d.cat            <- subset(d.base, exp == "Cat")
# d.cat$integrated <- as.factor(d.cat$integrated)
# d.cat$related    <- as.factor(d.cat$related)
# d.cat$related    <- droplevels(d.cat$rel)
# d.cat$n2num      <- as.factor(d.cat$n2num)
# d.cat$item       <- as.factor(d.cat$item)
# line             = rep(c("-"), times = 40, fill = 80)
# br               = "\n"
# 
# 
# # Below, designates various subsets of the original data file
# integ <- subset(d.cat, integrated   ==  "integ")
# unint <- subset(d.cat, integrated   ==  "unint")
# relat <- subset(d.cat, related      ==  "rel")
# unrel <- subset(d.cat, related      ==  "unrel")
# sing  <- subset(d.cat, n2num        ==  "sing")
# plur  <- subset(d.cat, n2num        ==  "plur")
# 
# #Below, additional subsetted groups
# relat.int.plur   <- subset(d.cat, related     == "rel"   & integrated == "integ" & n2num == "plur")
# relat.int.sing   <- subset(d.cat, related     == "rel"   & integrated == "integ" & n2num == "sing")
# relat.unint.plur <- subset(d.cat, related     == "rel"   & integrated == "unint" & n2num == "plur")
# relat.unint.sing <- subset(d.cat, related     == "rel"   & integrated == "unint" & n2num == "sing")
# unrel.int.plur   <- subset(d.cat, related     == "unrel" & integrated == "integ" & n2num == "plur")
# unrel.int.sing   <- subset(d.cat, related     == "unrel" & integrated == "integ" & n2num == "sing")
# unrel.unint.plur <- subset(d.cat, related     == "unrel" & integrated == "unint" & n2num == "plur")
# unrel.unint.sing <- subset(d.cat, related     == "unrel" & integrated == "unint" & n2num == "sing")
# relat.plur       <- subset(d.cat, related     == "rel"   & n2num      == "plur")
# relat.sing       <- subset(d.cat, related     == "rel"   & n2num      == "sing")
# unrel.plur       <- subset(d.cat, related     == "unrel" & n2num      == "plur")
# unrel.sing       <- subset(d.cat, related     == "unrel" & n2num      == "sing")
# integ.plur       <- subset(d.cat, integrated  == "integ" & n2num      == "plur")
# integ.sing       <- subset(d.cat, integrated  == "integ" & n2num      == "sing")
# unint.plur       <- subset(d.cat, integrated  == "unint" & n2num      == "plur")
# unint.sing       <- subset(d.cat, integrated  == "unint" & n2num      == "sing")
# integ.relat      <- subset(d.cat, integrated  == "integ" & related    == "rel")
# integ.unrel      <- subset(d.cat, integrated  == "integ" & related    == "unrel")
# unint.relat      <- subset(d.cat, integrated  == "unint" & related    == "rel")
# unint.unrel      <- subset(d.cat, integrated  == "unint" & related    == "unrel")
# 
# get_stars = function(p) {
#   stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
#   codes = c("***" , "**","*", ".", " ")
#   codes[stars]
# }
# options(scipen=1)
# 
# # ---------------------- CAT: SET UP OUTPUT FILE
# 
# sink("output/norming/CATEGORY_COORD norming.txt")
# cat("CATEGORY_COORD NORMING ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
# cat(rep(c("*"), times = 25, quote = F), br, br)
# 
# 
# # -------------------- CAT CORD: NUISANCE FACTORS
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** NUISANCE FACTORS **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- CAT CORD: PREPOSITION -------------------------------------------------------
# cat( br, rep(c("="), times = 50, quote = F), br)
# cat("PREPOSITION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.freq <- summary(aov(LogFreq.Prep ~ related + Error(item / related ), data = d.cat))
# p <- zapsmall(aov.freq[[2]][[1]][["Pr(>F)"]][1], digits=6)
# pval <- data.frame(
#   PVal = c(p, get_stars(p))
# )
# 
# Preposition  = c("Related","Unrelated","PVal")
# 
# prep.freq = data.frame(
#   c(
#     paste( round( mean( relat$LogFreq.Prep), 2), " (", round( sd( relat$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Prep), 2), " (", round( sd( unrel$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(prep.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(Preposition, prep.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lc <- summary(aov(LengthChar.Prep ~ related + Error(item / related ), data = d.cat))
# p <- zapsmall(aov.lc[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthChar.Prep), 2), " (", round( sd( relat$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Prep), 2), " (", round( sd( unrel$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.ph <- summary(aov(LengthPhon.Prep ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.ph[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthPhon.Prep), 2), " (", round( sd( relat$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Prep), 2), " (", round( sd( unrel$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.syl <- summary(aov(LengthSyll.Prep ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.syl[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthSyll.Prep), 2), " (", round( sd( relat$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Prep), 2), " (", round( sd( unrel$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syl. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- CAT CORD: ADJECTIVE -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ADJECTIVE", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.fra <- summary(aov(LogFreq.Adj ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.fra[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Adjective  = c("Related","Unrelated", "PVal")
# adj.freq = data.frame(
#   c(
#     paste( round( mean( relat$LogFreq.Adj), 2), " (", round( sd( relat$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Adj), 2), " (", round( sd( unrel$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(adj.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(Adjective, adj.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.cha <- summary(aov(LengthChar.Adj ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.cha[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthChar.Adj), 2), " (", round( sd( relat$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Adj), 2), " (", round( sd( unrel$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.pha <- summary(aov(LengthPhon.Adj ~ related + Error(item / related ), data = d.cat))
# p <- zapsmall(aov.pha[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthPhon.Adj), 2), " (", round( sd( relat$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Adj), 2), " (", round( sd( unrel$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.sya <- summary(aov(LengthSyll.Adj ~ related + Error(item / related ), data = d.cat))
# p <- zapsmall(aov.sya[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat$LengthSyll.Adj), 2), " (", round( sd( relat$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Adj), 2), " (", round( sd( unrel$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- CAT CORD: RELATED LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED LOCAL NOUN SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frsp <- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.frsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# RelatedLN  = c("Singular","Plural", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( relat.sing$LogFreq.N1), 2), " (", round( sd( relat.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LogFreq.N1), 2), " (", round( sd( relat.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(RelatedLN, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcsp <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.lcsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthChar.Noun), 2), " (", round( sd( relat.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LengthChar.Noun), 2), " (", round( sd( relat.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpsp <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.lpsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthPhon.Noun), 2), " (", round( sd( relat.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LengthPhon.Noun), 2), " (", round( sd( relat.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthSyll.Noun), 2), " (", round( sd( relat.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$LengthSyll.Noun), 2), " (", round( sd( relat.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- CAT CORD: UNRELATED LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED LOCAL NOUN SING. vs. PLUR.", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frsp <- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.frsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# UnrelatedLN  = c("Singular","Plural", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LogFreq.N1), 2), " (", round( sd( unrel.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LogFreq.N1), 2), " (", round( sd( unrel.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(UnrelatedLN, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcsp <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.lcsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthChar.Noun), 2), " (", round( sd( unrel.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthChar.Noun), 2), " (", round( sd( unrel.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpsp <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.lpsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthPhon.Noun), 2), " (", round( sd( unrel.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthPhon.Noun), 2), " (", round( sd( unrel.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthSyll.Noun), 2), " (", round( sd( unrel.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthSyll.Noun), 2), " (", round( sd( unrel.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- CAT CORD: RELATED vs UNRELATED SINGULAR LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs UNRELATED SINGULAR LOCAL NOUN", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frss <- summary(aov(LogFreq.N1 ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.frss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# LocalNoun  = c("Rel.Sing","Unrel.Sing", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( relat.sing$LogFreq.N1), 2), " (", round( sd( relat.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LogFreq.N1), 2), " (", round( sd( unrel.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(LocalNoun, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcss <- summary(aov(LengthChar.Noun ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.lcss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthChar.Noun), 2), " (", round( sd( relat.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LengthChar.Noun), 2), " (", round( sd( unrel.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpss <- summary(aov(LengthPhon.Noun ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.lpss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthPhon.Noun), 2), " (", round( sd( relat.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LengthPhon.Noun), 2), " (", round( sd( unrel.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat.sing$LengthSyll.Noun), 2), " (", round( sd( relat.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$LengthSyll.Noun), 2), " (", round( sd( unrel.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- CAT CORD: RELATED vs UNRELATED PLURAL LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs UNRELATED PLURAL LOCAL NOUN", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov.frss <- summary(aov(LogFreq.N1 ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.frss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# LocalNoun  = c("Rel.Plur","Unrel.Plur", "PVal")
# noun.freq = data.frame(
#   c(
#     paste( round( mean( relat.plur$LogFreq.N1), 2), " (", round( sd( relat.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LogFreq.N1), 2), " (", round( sd( unrel.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(noun.freq)[1] <- "Frequency" 
# freq.means <- cbind.data.frame(LocalNoun, noun.freq)
# # ----------------------------- ::CHAR. LENGTH------------
# aov.lcss <- summary(aov(LengthChar.Noun ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.lcss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# char.len = data.frame(
#   c(
#     paste( round( mean( relat.plur$LengthChar.Noun), 2), " (", round( sd( relat.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthChar.Noun), 2), " (", round( sd( unrel.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(char.len)[1] <- "Char. Length" 
# freq.means <- cbind.data.frame(freq.means, char.len)
# # ----------------------------- ::PHON. LENGTH------------
# aov.lpss <- summary(aov(LengthPhon.Noun ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.lpss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# phon.len = data.frame(
#   c(
#     paste( round( mean( relat.plur$LengthPhon.Noun), 2), " (", round( sd( relat.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthPhon.Noun), 2), " (", round( sd( unrel.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(phon.len)[1] <- "Phon. Length" 
# freq.means <- cbind.data.frame(freq.means, phon.len)
# # ----------------------------- ::SYLL. LENGTH------------
# aov.lssp <- summary(aov(LengthSyll.Noun ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.lssp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# syll.len = data.frame(
#   c(
#     paste( round( mean( relat.plur$LengthSyll.Noun), 2), " (", round( sd( relat.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthSyll.Noun), 2), " (", round( sd( unrel.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(syll.len)[1] <- "Syll. Length" 
# freq.means <- cbind.data.frame(freq.means, syll.len)
# print(freq.means)
# # -------------------- CAT CORD: 2 x 2 LOCAL NOUN ANOVAs -------------------------------------------------------
# # ----------------------------- ::FREQUENCY------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA LOG FREQUENCY ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.fr22 <- summary(aov(LogFreq.N1 ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.cat))
# p.rel <- zapsmall(aov.fr22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.freq.rel = data.frame(
#   c(
#     paste( round( mean( relat$LogFreq.N1), 2), " (", round( sd( relat$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.N1), 2), " (", round( sd( unrel$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.freq.rel)[1] <- "Frequency" 
# ds.rel <- cbind.data.frame(Condition, noun.freq.rel)
# 
# 
# p.num <- zapsmall(aov.fr22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.freq.num = data.frame(
#   c(
#     paste( round( mean( sing$LogFreq.N1), 2), " (", round( sd( sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( plur$LogFreq.N1), 2), " (", round( sd( plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.freq.num)[1] <- "Frequency" 
# ds.num <- cbind.data.frame(Condition, noun.freq.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# # ----------------------------- ::CHAR.LENGTH------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA CHARACTER LENGTH ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.cl22 <- summary(aov(LengthChar.Noun ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.cat))
# p.rel <- zapsmall(aov.cl22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.char.rel = data.frame(
#   c(
#     paste( round( mean( relat$LengthChar.Noun), 2), " (", round( sd( relat$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Noun), 2), " (", round( sd( unrel$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.char.rel)[1] <- "Char. Length" 
# ds.rel <- cbind.data.frame(Condition, noun.char.rel)
# 
# 
# p.num <- zapsmall(aov.cl22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.char.num = data.frame(
#   c(
#     paste( round( mean( sing$LengthChar.Noun), 2), " (", round( sd( sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( plur$LengthChar.Noun), 2), " (", round( sd( plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.char.num)[1] <- "Char. Length" 
# ds.num <- cbind.data.frame(Condition, noun.char.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# # ----------------------------- ::PHON.LENGTH------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA PHONEME LENGTH ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.pl22 <- summary(aov(LengthPhon.Noun ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.cat))
# p.rel <- zapsmall(aov.pl22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.phon.rel = data.frame(
#   c(
#     paste( round( mean( relat$LengthPhon.Noun), 2), " (", round( sd( relat$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Noun), 2), " (", round( sd( unrel$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.phon.rel)[1] <- "Phon. Length" 
# ds.rel <- cbind.data.frame(Condition, noun.phon.rel)
# 
# 
# p.num <- zapsmall(aov.pl22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.phon.num = data.frame(
#   c(
#     paste( round( mean( sing$LengthPhon.Noun), 2), " (", round( sd( sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( plur$LengthPhon.Noun), 2), " (", round( sd( plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.phon.num)[1] <- "Phon. Length" 
# ds.num <- cbind.data.frame(Condition, noun.phon.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# # ----------------------------- ::SYLL.LENGTH------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2x2 ANOVA SYLLABLE LENGTH ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.sl22 <- summary(aov(LengthSyll.Noun ~ (related + n2num)^2 + Error(item / (related * n2num)), data = d.cat))
# p.rel <- zapsmall(aov.sl22[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Related","Unrelelated","PVal")
# noun.phon.rel = data.frame(
#   c(
#     paste( round( mean( relat$LengthSyll.Noun), 2), " (", round( sd( relat$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Noun), 2), " (", round( sd( unrel$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p.rel, 3), get_stars(p.rel))
#   ))
# colnames(noun.phon.rel)[1] <- "Syll. Length" 
# ds.rel <- cbind.data.frame(Condition, noun.phon.rel)
# 
# 
# p.num <- zapsmall(aov.sl22[[3]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# Condition  = c("Singular","Plural","PVal")
# noun.phon.num = data.frame(
#   c(
#     paste( round( mean( sing$LengthSyll.Noun), 2), " (", round( sd( sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( plur$LengthSyll.Noun), 2), " (", round( sd( plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p.num, 3), get_stars(p.num))
#   ))
# colnames(noun.phon.num)[1] <- "Syll. Length" 
# ds.num <- cbind.data.frame(Condition, noun.phon.num)
# 
# ds <- rbind(ds.rel, ds.num)
# print(ds)
# 
# # -------------------- CAT CORD: RELATEDNESS NORMING-----------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** RELATEDNESS  **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- CAT CORD:  2 x 2 ANOVA RELATEDNESS -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 ANOVA RELATEDNESS", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# rel22 <- summary(aov(RelatedHL ~ related * n2num + Error(item / related * n2num), data = d.cat))
# print(rel22)
# # -------------------- CAT CORD: RELATEDNESS RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.rel <- summary(aov(RelatedHL ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.rel[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Mean Related", "Mean Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$RelatedHL), 2), " (", round( sd( relat$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel$RelatedHL), 2), " (", round( sd( unrel$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "Relatedness" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- CAT CORD: RELATEDNESS SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$RelatedHL), 2), " (", round( sd( relat.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$RelatedHL), 2), " (", round( sd( relat.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Related" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Unrelated" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- CAT CORD: RELATEDNESS SING vs. UNRELATEDNESS SING -------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(RelatedHL ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Related.Sing","Unrelated.Sing", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$RelatedHL), 2), " (", round( sd( relat.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Total" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # -------------------- CAT CORD: RELATEDNESS PLUR vs. UNRELATEDNESS PLUR -----------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(RelatedHL ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( relat.plur$RelatedHL), 2), " (", round( sd( relat.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Total" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# 
# # -------------------- CAT CORD: INTEGRATION NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** SEMANTIC INTEGRATION **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- CAT CORD:  2 x 2 ANOVA INTEGRATION -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 ANOVA INTEGRATION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# int22 <- summary(aov(Integrated ~  related * n2num + Error(item /  related * n2num), data = d.cat))
# print(int22)
# # -------------------- CAT CORD: INT: RELATED vs. UNRELATED -------------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INT: RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunint <- summary(aov(Integrated ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.intunint[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$Integrated), 2), " (", round( sd( relat$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel$Integrated), 2), " (", round( sd( unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "Total" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- CAT CORD: INTEGRATION REL. SING vs. REL. PLUR -------------------------------------------------------
# # ----------------------------- ::REL SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INT. REL SING vs. REL. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.intsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intsp)[1] <- "Integrated" 
# int.means <- cbind.data.frame(N2Num, intsp)
# 
# print(int.means)
# # ----------------------------- ::UNREL SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INT. UNREL SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.unintsp <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unintsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unintsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unintsp)[1] <- "Integrated" 
# unint.means <- cbind.data.frame(N2Num, unintsp)
# 
# print(unint.means)
# # -------------------- CAT CORD: RELATED SING/PLUR vs. UNRELATED SING/PLUR -------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INT: RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunintss <- summary(aov(Integrated ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.intunintss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Related.Sing","Unrelated.Sing", "PVal")
# intunintss = data.frame(
#   c(
#     paste( round( mean( relat.sing$Integrated), 2), " (", round( sd( relat.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintss)[1] <- "Total" 
# intunintss.means <- cbind.data.frame(N2Num, intunintss)
# 
# print(intunintss.means)
# # -------------------- CAT CORD: INTEGRATED PLUR vs. UNINTEGRATED PLUR -------------------------------------------------------
# # ----------------------------- ::INT. PLUR vs UINT. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INT: RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.intunintpp <- summary(aov(Integrated ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.intunintpp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intunintpp = data.frame(
#   c(
#     paste( round( mean( relat.plur$Integrated), 2), " (", round( sd( relat.plur$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintpp)[1] <- "Total" 
# intunintpp.means <- cbind.data.frame(N2Num, intunintpp)
# 
# print(intunintpp.means)
# 
# # -------------------- CAT CORD: ASSOCIATION NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** ASSOCIATION **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- CAT CORD:  2 x 2 ANOVA ASSOCIATION -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 ANOVA ASSOCIATION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# ass22 <- summary(aov(AssArc.H.L ~ related * n2num + Error(item / related * n2num), data = d.cat))
# print(ass22)
# # -------------------- CAT CORD: ASSOCIATED RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION: RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.ass <- summary(aov(AssArc.H.L ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.ass[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Mean Related", "Mean Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$AssArc.H.L), 2), " (", round( sd( relat$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel$AssArc.H.L), 2), " (", round( sd( unrel$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "AssArc HL" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- CAT CORD: ASSOCIATED SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION: RELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$AssArc.H.L), 2), " (", round( sd( relat.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$AssArc.H.L), 2), " (", round( sd( relat.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "AssArc HL" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION: UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$AssArc.H.L), 2), " (", round( sd( unrel.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$AssArc.H.L), 2), " (", round( sd( unrel.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "AssArc HL" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- CAT CORD: ASSOCIATED: REL SING vs. UNREL SING -------------------------------------------------
# # ----------------------------- ::REL SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATED: REL SING vs. UNREL SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunintss <- summary(aov(Integrated ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.intunintss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Related.Sing","Unrelated.Sing", "PVal")
# intunintss = data.frame(
#   c(
#     paste( round( mean( relat.sing$Integrated), 2), " (", round( sd( relat.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintss)[1] <- "Total" 
# intunintss.means <- cbind.data.frame(N2Num, intunintss)
# 
# print(intunintss.means)
# # -------------------- CAT CORD: ASSOCIATED: REL PLUR vs. UNREL PLUR -------------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATED: REL PLUR vs. UNREL PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.intunintpp <- summary(aov(Integrated ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.intunintpp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intunintpp = data.frame(
#   c(
#     paste( round( mean( relat.plur$Integrated), 2), " (", round( sd( relat.plur$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintpp)[1] <- "Total" 
# intunintpp.means <- cbind.data.frame(N2Num, intunintpp)
# 
# print(intunintpp.means)
# 
# # -------------------- CAT CORD: PLAUSIBILITY NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** PLAUSIBILITY  **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # -------------------- CAT CORD:  2 x 2 x 2 ANOVA PLAUSIBILITY -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 2 ANOVA PLAUSIBILITY", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# plaus22 <- summary(aov(Plausibility ~   related * n2num + Error(item /  related * n2num), data = d.cat))
# print(plaus22)
# # -------------------- CAT CORD: PLAUS: RELATED vs. UNRELATED -------------------------------------------------------
# 
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunint <- summary(aov(Plausibility ~ related + Error(item / related), data = d.cat))
# p <- zapsmall(aov.intunint[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Relatedness  = c("Related","Unrelated", "PVal")
# relunrel = data.frame(
#   c(
#     paste( round( mean( relat$Plausibility), 2), " (", round( sd( relat$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel$Plausibility), 2), " (", round( sd( unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relunrel)[1] <- "Plaus.Total" 
# relunrel.means <- cbind.data.frame(Relatedness, relunrel)
# 
# print(relunrel.means)
# # -------------------- CAT CORD: PLAUS: SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::REL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: RELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.relsp <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = relat))
# p <- zapsmall(aov.relsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# relsp = data.frame(
#   c(
#     paste( round( mean( relat.sing$Plausibility), 2), " (", round( sd( relat.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( relat.plur$Plausibility), 2), " (", round( sd( relat.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(relsp)[1] <- "Plaus. Total" 
# rel.means <- cbind.data.frame(N2Num, relsp)
# 
# print(rel.means)
# # ----------------------------- ::UNREL. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.unrelsp <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov.unrelsp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# unrelsp = data.frame(
#   c(
#     paste( round( mean( unrel.sing$Plausibility), 2), " (", round( sd( unrel.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Plausibility), 2), " (", round( sd( unrel.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(unrelsp)[1] <- "Plaus. Total" 
# unrel.means <- cbind.data.frame(N2Num, unrelsp)
# 
# print(unrel.means)
# # -------------------- CAT CORD: PLAUS: REL SING vs. UNREL SING -------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: REL SING vs. UNREL SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov.intunintss <- summary(aov(Plausibility ~ related + Error(item / related), data = sing))
# p <- zapsmall(aov.intunintss[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Related.Sing","Unrelated.Sing", "PVal")
# intunintss = data.frame(
#   c(
#     paste( round( mean( relat.sing$Integrated), 2), " (", round( sd( relat.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintss)[1] <- "Plaus. Total" 
# intunintss.means <- cbind.data.frame(N2Num, intunintss)
# 
# print(intunintss.means)
# # -------------------- CAT CORD: PLAUS: REL PLUR vs. UNREL PLUR -------------------------------------------------------
# # ----------------------------- ::INT. PLUR vs UINT. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUS: REL PLUR vs. UNREL PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov.intunintpp <- summary(aov(Plausibility ~ related + Error(item / related), data = plur))
# p <- zapsmall(aov.intunintpp[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# N2Num  = c("Singular","Plural", "PVal")
# intunintpp = data.frame(
#   c(
#     paste( round( mean( relat.plur$Plausibility), 2), " (", round( sd( relat.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Plausibility), 2), " (", round( sd( unrel.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(intunintpp)[1] <- "Plaus. Total" 
# intunintpp.means <- cbind.data.frame(N2Num, intunintpp)
# 
# print(intunintpp.means)
# 
# sink()
# 
# # ==================================================================================================
# rm(list = ls()) # clears environment
# library(languageR) # calls languageR library
# get_stars = function(p) {
#   stars = findInterval(p, c(0, 0.001, 0.01, 0.05, 0.1))
#   codes = c("***" , "**","*", ".", " ")
#   codes[stars]
# }
# options(scipen=1)
# 
# # ---------------------- PROP: SET UP INPUT FILE(S) -----------------------------------
# d.base           <- read.csv("data/allregdata.csv")
# names(d.base)[names(d.base) == 'LengthSylHead'] <- 'LengthSyll.Head'
# names(d.base)[names(d.base) == 'LogFr.Head'] <- 'LogFreq.Head'
# # ---------------------- PROP: SUBSETS  ---------
# d.prop            <- subset(d.base, exp == "Prop")
# d.prop$integrated <- as.factor(d.prop$integrated)
# d.prop$related    <- as.factor(d.prop$related)
# d.prop$related    <- droplevels(d.prop$rel)
# d.prop$n2num      <- as.factor(d.prop$n2num)
# d.prop$item       <- as.factor(d.prop$item)
# line             = rep(c("-"), times = 40, fill = 80)
# br               = "\n"
# 
# 
# # Below, designates various subsets of the original data file
# 
# attrb <- subset(d.prop, related      == "rel")
# assoc <- subset(d.prop, related      == "assoc")
# unrel <- subset(d.prop, related      == "unrel")
# sing  <- subset(d.prop, n2num        == "sing")
# plur  <- subset(d.prop, n2num        == "plur")
# 
# #Below, additional subsetted groups
# 
# attrb.assoc      <- subset(d.prop, related     != "unrel")
# attrb.assoc.plur <- subset(d.prop, related     != "unrel" & n2num == "plur")
# attrb.assoc.sing <- subset(d.prop, related     != "unrel" & n2num == "sing")
# attrb.unrel      <- subset(d.prop, related     != "assoc")
# attrb.unrel.plur <- subset(d.prop, related     != "assoc" & n2num == "plur")
# attrb.unrel.sing <- subset(d.prop, related     != "assoc" & n2num == "sing")
# assoc.unrel      <- subset(d.prop, related     != "rel")
# assoc.unrel.plur <- subset(d.prop, related     != "rel" & n2num == "plur")
# assoc.unrel.sing <- subset(d.prop, related     != "rel" & n2num == "sing")
# attrb.plur       <- subset(d.prop, related     == "rel"   & n2num   == "plur")
# attrb.sing       <- subset(d.prop, related     == "rel"   & n2num   == "sing")
# assoc.plur       <- subset(d.prop, related     == "assoc" & n2num   == "plur")
# assoc.sing       <- subset(d.prop, related     == "assoc" & n2num   == "sing")
# unrel.plur       <- subset(d.prop, related     == "unrel" & n2num   == "plur")
# unrel.sing       <- subset(d.prop, related     == "unrel" & n2num   == "sing")
# 
# 
# 
# # --------------------- PROP: SET UP OUTPUT FILE
# 
# sink("output/norming/PROP norming.txt")
# cat("PROPERTY NORMING ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
# cat(rep(c("*"), times = 25, quote = F), br)
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** NUISANCE FACTORS **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# 
# # ---------------------- PROP: PREPOSITION -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PREPOSITION", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# 
# aov1<- summary(aov(LogFreq.Prep ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes","Unrelated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LogFreq.Prep), 2), " (", round( sd( attrb$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Prep), 2), " (", round( sd( unrel$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means1 <- cbind.data.frame(Condition,ds)
# 
# ###
# 
# aov2 <- summary(aov(LogFreq.Prep ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes","Associated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LogFreq.Prep), 2), " (", round( sd( attrb$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( mean( assoc$LogFreq.Prep), 2), " (", round( sd( assoc$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means2 <- cbind.data.frame(Condition, ds)
# 
# ###
# 
# aov3 <- summary(aov(LogFreq.Prep ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated","Unrel","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LogFreq.Prep), 2), " (", round( sd( assoc$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Prep), 2), " (", round( sd( unrel$LogFreq.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means3 <- cbind.data.frame(Condition, ds)
# # ----------------------------- ::CHAR. LENGTH------------
# 
# aov1 <- summary(aov(LengthChar.Prep ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthChar.Prep), 2), " (", round( sd( attrb$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Prep), 2), " (", round( sd( unrel$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means1 <- cbind.data.frame(means1,ds)
# 
# ###
# 
# aov2 <- summary(aov(LengthChar.Prep ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthChar.Prep), 2), " (", round( sd( attrb$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( mean( assoc$LengthChar.Prep), 2), " (", round( sd( assoc$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means2 <- cbind.data.frame(means2, ds)
# 
# ###
# 
# aov3 <- summary(aov(LengthChar.Prep ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LengthChar.Prep), 2), " (", round( sd( assoc$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Prep), 2), " (", round( sd( unrel$LengthChar.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means3 <- cbind.data.frame(means3, ds)
# # ----------------------------- ::PHON. LENGTH------------
# # ----------------------------- ::PHON. LENGTH------------
# aov1 <- summary(aov(LengthPhon.Prep ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthPhon.Prep), 2), " (", round( sd( attrb$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Prep), 2), " (", round( sd( unrel$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means1 <- cbind.data.frame(means1,ds)
# 
# ###
# 
# aov2 <- summary(aov(LengthPhon.Prep ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthPhon.Prep), 2), " (", round( sd( attrb$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( mean( assoc$LengthPhon.Prep), 2), " (", round( sd( assoc$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means2 <- cbind.data.frame(means2, ds)
# 
# ###
# 
# aov3 <- summary(aov(LengthPhon.Prep ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LengthPhon.Prep), 2), " (", round( sd( assoc$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Prep), 2), " (", round( sd( unrel$LengthPhon.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means3 <- cbind.data.frame(means3, ds)
# # ----------------------------- ::SYLL. LENGTH------------
# aov1 <- summary(aov(LengthSyll.Prep ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthSyll.Prep), 2), " (", round( sd( attrb$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Prep), 2), " (", round( sd( unrel$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means1 <- cbind.data.frame(means1,ds)
# print(means1)
# ###
# 
# aov2 <- summary(aov(LengthSyll.Prep ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthSyll.Prep), 2), " (", round( sd( attrb$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( mean( assoc$LengthSyll.Prep), 2), " (", round( sd( assoc$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means2 <- cbind.data.frame(means2, ds)
# print(means2)
# ###
# 
# aov3 <- summary(aov(LengthSyll.Prep ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LengthSyll.Prep), 2), " (", round( sd( assoc$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Prep), 2), " (", round( sd( unrel$LengthSyll.Prep), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means3 <- cbind.data.frame(means3, ds)
# print(means3)
# # ----------------------PROP: ADJECTIVE -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ADJECTIVE", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov1<- summary(aov(LogFreq.Adj ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes","Unrelated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LogFreq.Adj), 2), " (", round( sd( attrb$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Adj), 2), " (", round( sd( unrel$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means1 <- cbind.data.frame(Condition,ds)
# 
# ###
# 
# aov2 <- summary(aov(LogFreq.Adj ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes","Associated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LogFreq.Adj), 2), " (", round( sd( attrb$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( mean( assoc$LogFreq.Adj), 2), " (", round( sd( assoc$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means2 <- cbind.data.frame(Condition, ds)
# 
# ###
# 
# aov3 <- summary(aov(LogFreq.Adj ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated","Unrel","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LogFreq.Adj), 2), " (", round( sd( assoc$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LogFreq.Adj), 2), " (", round( sd( unrel$LogFreq.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means3 <- cbind.data.frame(Condition, ds)
# # ----------------------------- ::CHAR. LENGTH------------
# 
# aov1 <- summary(aov(LengthChar.Adj ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthChar.Adj), 2), " (", round( sd( attrb$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Adj), 2), " (", round( sd( unrel$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means1 <- cbind.data.frame(means1,ds)
# 
# ###
# 
# aov2 <- summary(aov(LengthChar.Adj ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthChar.Adj), 2), " (", round( sd( attrb$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( mean( assoc$LengthChar.Adj), 2), " (", round( sd( assoc$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means2 <- cbind.data.frame(means2, ds)
# 
# ###
# 
# aov3 <- summary(aov(LengthChar.Adj ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LengthChar.Adj), 2), " (", round( sd( assoc$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthChar.Adj), 2), " (", round( sd( unrel$LengthChar.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means3 <- cbind.data.frame(means3, ds)
# # ----------------------------- ::PHON. LENGTH------------
# aov1 <- summary(aov(LengthPhon.Adj ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthPhon.Adj), 2), " (", round( sd( attrb$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Adj), 2), " (", round( sd( unrel$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means1 <- cbind.data.frame(means1,ds)
# 
# ###
# 
# aov2 <- summary(aov(LengthPhon.Adj ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthPhon.Adj), 2), " (", round( sd( attrb$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( mean( assoc$LengthPhon.Adj), 2), " (", round( sd( assoc$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means2 <- cbind.data.frame(means2, ds)
# 
# ###
# 
# aov3 <- summary(aov(LengthPhon.Adj ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LengthPhon.Adj), 2), " (", round( sd( assoc$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthPhon.Adj), 2), " (", round( sd( unrel$LengthPhon.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means3 <- cbind.data.frame(means3, ds)
# # ----------------------------- ::SYLL. LENGTH------------
# aov1 <- summary(aov(LengthSyll.Adj ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthSyll.Adj), 2), " (", round( sd( attrb$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Adj), 2), " (", round( sd( unrel$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means1 <- cbind.data.frame(means1,ds)
# print(means1)
# ###
# 
# aov2 <- summary(aov(LengthSyll.Adj ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$LengthSyll.Adj), 2), " (", round( sd( attrb$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( mean( assoc$LengthSyll.Adj), 2), " (", round( sd( assoc$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means2 <- cbind.data.frame(means2, ds)
# print(means2)
# ###
# 
# aov3 <- summary(aov(LengthSyll.Adj ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$LengthSyll.Adj), 2), " (", round( sd( assoc$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( mean( unrel$LengthSyll.Adj), 2), " (", round( sd( unrel$LengthSyll.Adj), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means3 <- cbind.data.frame(means3, ds)
# print(means3)
# # ----------------------PROP: LOCAL NOUN -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("LOCAL NOUN SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# # ----------------------------- ::FREQUENCY------------
# aov1<- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num ), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attrib.Sing","Attrib.Plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$LogFreq.N1), 2), " (", round( sd( attrb.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$LogFreq.N1), 2), " (", round( sd( attrb.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means1 <- cbind.data.frame(Condition,ds)
# 
# ###
# 
# aov2 <- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num ), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Assoc.Sing","Assoc.Plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$LogFreq.N1), 2), " (", round( sd( assoc.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$LogFreq.N1), 2), " (", round( sd( assoc.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means2 <- cbind.data.frame(Condition, ds)
# 
# ###
# 
# aov3 <- summary(aov(LogFreq.N1 ~ n2num + Error(item / n2num ), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Unrel.Sing","Unrel.Plur","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LogFreq.N1), 2), " (", round( sd( unrel.sing$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LogFreq.N1), 2), " (", round( sd( unrel.plur$LogFreq.N1), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Frequency" 
# means3 <- cbind.data.frame(Condition, ds)
# # ----------------------------- ::CHAR. LENGTH------------
# 
# aov1 <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num ), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$LengthChar.Noun), 2), " (", round( sd( attrb.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$LengthChar.Noun), 2), " (", round( sd( attrb.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means1 <- cbind.data.frame(means1,ds)
# 
# ###
# 
# aov2 <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num ), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$LengthChar.Noun), 2), " (", round( sd( assoc.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$LengthChar.Noun), 2), " (", round( sd( assoc.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means2 <- cbind.data.frame(means2, ds)
# 
# ###
# 
# aov3 <- summary(aov(LengthChar.Noun ~ n2num + Error(item / n2num ), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthChar.Noun), 2), " (", round( sd( unrel.sing$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthChar.Noun), 2), " (", round( sd( unrel.plur$LengthChar.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Char. Length" 
# means3 <- cbind.data.frame(means3, ds)
# # ----------------------------- ::PHON. LENGTH------------
# aov1 <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num ), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$LengthPhon.Noun), 2), " (", round( sd( attrb.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$LengthPhon.Noun), 2), " (", round( sd( attrb.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means1 <- cbind.data.frame(means1,ds)
# 
# ###
# 
# aov2 <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num ), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$LengthPhon.Noun), 2), " (", round( sd( assoc.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$LengthPhon.Noun), 2), " (", round( sd( assoc.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means2 <- cbind.data.frame(means2, ds)
# 
# ###
# 
# aov3 <- summary(aov(LengthPhon.Noun ~ n2num + Error(item / n2num ), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthPhon.Noun), 2), " (", round( sd( unrel.sing$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthPhon.Noun), 2), " (", round( sd( unrel.plur$LengthPhon.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Phon. Length" 
# means3 <- cbind.data.frame(means3, ds)
# # ----------------------------- ::SYLL. LENGTH------------
# aov1 <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num ), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$LengthSyll.Noun), 2), " (", round( sd( attrb.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$LengthSyll.Noun), 2), " (", round( sd( attrb.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means1 <- cbind.data.frame(means1,ds)
# print(means1)
# ###
# 
# aov2 <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num ), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$LengthSyll.Noun), 2), " (", round( sd( assoc.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$LengthSyll.Noun), 2), " (", round( sd( assoc.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means2 <- cbind.data.frame(means2, ds)
# print(means2)
# ###
# 
# aov3 <- summary(aov(LengthSyll.Noun ~ n2num + Error(item / n2num ), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$LengthSyll.Noun), 2), " (", round( sd( unrel.sing$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$LengthSyll.Noun), 2), " (", round( sd( unrel.plur$LengthSyll.Noun), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Syll. Length" 
# means3 <- cbind.data.frame(means3, ds)
# print(means3)
# 
# # --------------------- PROP: RELATEDNESS NORMING----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** RELATEDNESS FACTORS **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # --------------------- PROP:  2 x 3 ANOVA RELATEDNESS -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("2 X 3 ANOVA RELATEDNESS", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# rel22 <- summary(aov(RelatedHL ~ related * n2num + Error(item / related * n2num), data = d.prop))
# print(rel22)
# # --------------------- PROP: RELATEDNESS RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED vs. UNRELATED", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(RelatedHL ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes","Unrelated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$RelatedHL), 2), " (", round( sd( attrb$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel$RelatedHL), 2), " (", round( sd( unrel$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(RelatedHL ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes","Associated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$RelatedHL), 2), " (", round( sd( attrb$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( assoc$RelatedHL), 2), " (", round( sd( assoc$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(RelatedHL ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated","Unrel","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$RelatedHL), 2), " (", round( sd( assoc$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel$RelatedHL), 2), " (", round( sd( unrel$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: RELATEDNESS SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::ATTRB. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ATTRIBUTE SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov1 <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$RelatedHL), 2), " (", round( sd( attrb.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$RelatedHL), 2), " (", round( sd( attrb.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition, ds)
# print(means1)
# # ----------------------------- ::ASSOC. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# aov2 <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$RelatedHL), 2), " (", round( sd( assoc.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$RelatedHL), 2), " (", round( sd( assoc.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# # ----------------------------- ::UNRELATED SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov3 <- summary(aov(RelatedHL ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: RELATEDNESS SING vs. UNRELATEDNESS SING -------------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(RelatedHL ~ related + Error(item / related ), data = attrb.unrel.sing))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.sing","Unrelated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$RelatedHL), 2), " (", round( sd( attrb.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(RelatedHL ~ related + Error(item / related ), data = attrb.assoc.sing))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.sing","Associated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$RelatedHL), 2), " (", round( sd( attrb.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( assoc.sing$RelatedHL), 2), " (", round( sd( assoc.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(RelatedHL ~ related + Error(item / related ), data = assoc.unrel.sing))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.sing","Unrelated.sing","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$RelatedHL), 2), " (", round( sd( assoc.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$RelatedHL), 2), " (", round( sd( unrel.sing$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: RELATEDNESS PLUR vs. UNRELATEDNESS PLUR -------------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(RelatedHL ~ related + Error(item / related ), data = attrb.unrel.plur))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.plur","Unrelated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$RelatedHL), 2), " (", round( sd( attrb.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(RelatedHL ~ related + Error(item / related ), data = attrb.assoc.plur))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.plur","Associated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$RelatedHL), 2), " (", round( sd( attrb.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$RelatedHL), 2), " (", round( sd( assoc.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(RelatedHL ~ related + Error(item / related ), data = assoc.unrel.plur))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.plur","Unrelated.plur","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.plur$RelatedHL), 2), " (", round( sd( assoc.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$RelatedHL), 2), " (", round( sd( unrel.plur$RelatedHL), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# 
# # --------------------- PROP: INTEGRATION NORMING ----------------------------------------------------
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** INTEGRATION FACTORS **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # --------------------- PROP: INTEGRATION RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("INTEGRATED RELATED vs. UNRELATED ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(Integrated ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes","Unrelated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$Integrated), 2), " (", round( sd( attrb$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel$Integrated), 2), " (", round( sd( unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(Integrated ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes","Associated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$Integrated), 2), " (", round( sd( attrb$Integrated), 2), ")", sep = ""),
#     paste( round( mean( assoc$Integrated), 2), " (", round( sd( assoc$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(Integrated ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated","Unrel","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$Integrated), 2), " (", round( sd( assoc$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel$Integrated), 2), " (", round( sd( unrel$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: INTEGRATION SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::ATTRB. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ATTRIBUTE SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov1 <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$Integrated), 2), " (", round( sd( attrb.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$Integrated), 2), " (", round( sd( attrb.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition, ds)
# print(means1)
# # ----------------------------- ::ASSOC. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# aov2 <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$Integrated), 2), " (", round( sd( assoc.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$Integrated), 2), " (", round( sd( assoc.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# # ----------------------------- ::UNRELATED SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov3 <- summary(aov(Integrated ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: INTEGRATION SING vs.  SING -------------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(Integrated ~ related + Error(item / related ), data = attrb.unrel.sing))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.sing","Unrelated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$Integrated), 2), " (", round( sd( attrb.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(Integrated ~ related + Error(item / related ), data = attrb.assoc.sing))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.sing","Associated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$Integrated), 2), " (", round( sd( attrb.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( assoc.sing$Integrated), 2), " (", round( sd( assoc.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(Integrated ~ related + Error(item / related ), data = assoc.unrel.sing))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.sing","Unrelated.sing","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$Integrated), 2), " (", round( sd( assoc.sing$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Integrated), 2), " (", round( sd( unrel.sing$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: INTEGRATION PLUR vs.  PLUR -------------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(Integrated ~ related + Error(item / related ), data = attrb.unrel.plur))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.plur","Unrelated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$Integrated), 2), " (", round( sd( attrb.plur$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(Integrated ~ related + Error(item / related ), data = attrb.assoc.plur))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.plur","Associated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$Integrated), 2), " (", round( sd( attrb.plur$Integrated), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$Integrated), 2), " (", round( sd( assoc.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(Integrated ~ related + Error(item / related ), data = assoc.unrel.plur))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.plur","Unrelated.plur","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.plur$Integrated), 2), " (", round( sd( assoc.plur$Integrated), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Integrated), 2), " (", round( sd( unrel.plur$Integrated), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# 
# # --------------------- PROP: ASSOCIATION NORMING ----------------------------------------------------
# 
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** ASSOCIATION NORMING **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # --------------------- PROP: ASSOCIATION RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat(" RELATED vs. UNRELATED ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes","Unrelated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$AssArc.H.L), 2), " (", round( sd( attrb$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel$AssArc.H.L), 2), " (", round( sd( unrel$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes","Associated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$AssArc.H.L), 2), " (", round( sd( attrb$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( assoc$AssArc.H.L), 2), " (", round( sd( assoc$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated","Unrel","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$AssArc.H.L), 2), " (", round( sd( assoc$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel$AssArc.H.L), 2), " (", round( sd( unrel$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: ASSOCIATION SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::ATTRB. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ATTRIBUTE SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov1 <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$AssArc.H.L), 2), " (", round( sd( attrb.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$AssArc.H.L), 2), " (", round( sd( attrb.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition, ds)
# print(means1)
# # ----------------------------- ::ASSOC. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ASSOCIATION SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# aov2 <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$AssArc.H.L), 2), " (", round( sd( assoc.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$AssArc.H.L), 2), " (", round( sd( assoc.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# # ----------------------------- ::UNRELATED SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov3 <- summary(aov(AssArc.H.L ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$AssArc.H.L), 2), " (", round( sd( unrel.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$AssArc.H.L), 2), " (", round( sd( unrel.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: ASSOCIATION SING vs.  SING -------------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = attrb.unrel.sing))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.sing","Unrelated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$AssArc.H.L), 2), " (", round( sd( attrb.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$AssArc.H.L), 2), " (", round( sd( unrel.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = attrb.assoc.sing))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.sing","Associated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$AssArc.H.L), 2), " (", round( sd( attrb.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( assoc.sing$AssArc.H.L), 2), " (", round( sd( assoc.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = assoc.unrel.sing))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.sing","Unrelated.sing","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$AssArc.H.L), 2), " (", round( sd( assoc.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$AssArc.H.L), 2), " (", round( sd( unrel.sing$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: ASSOCIATION PLUR vs.  PLUR -------------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = attrb.unrel.plur))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.plur","Unrelated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$AssArc.H.L), 2), " (", round( sd( attrb.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$AssArc.H.L), 2), " (", round( sd( unrel.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = attrb.assoc.plur))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.plur","Associated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$AssArc.H.L), 2), " (", round( sd( attrb.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$AssArc.H.L), 2), " (", round( sd( assoc.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(AssArc.H.L ~ related + Error(item / related ), data = assoc.unrel.plur))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.plur","Unrelated.plur","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.plur$AssArc.H.L), 2), " (", round( sd( assoc.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$AssArc.H.L), 2), " (", round( sd( unrel.plur$AssArc.H.L), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# 
# # --------------------- PROP: PLAUSIBILTY NORMING ----------------------------------------------------
# 
# cat( br, rep(c("_"), times = 50, quote = F), br)
# cat( "*********************************** PLAUSIBILITY NORMING **************************", sep = "")
# cat( br, rep(c("-"), times = 50, quote = F), br)
# # --------------------- PROP: PLAUSIBILITY RELATED  vs. UNRELATED -------------------------------------------------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat(" RELATED vs. UNRELATED ", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(Plausibility ~ related + Error(item / related ), data = attrb.unrel))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes","Unrelated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$Plausibility), 2), " (", round( sd( attrb$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel$Plausibility), 2), " (", round( sd( unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(Plausibility ~ related + Error(item / related ), data = attrb.assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes","Associated","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb$Plausibility), 2), " (", round( sd( attrb$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( assoc$Plausibility), 2), " (", round( sd( assoc$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(Plausibility ~ related + Error(item / related ), data = assoc.unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated","Unrel","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc$Plausibility), 2), " (", round( sd( assoc$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel$Plausibility), 2), " (", round( sd( unrel$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: PLAUSIBILITY SING vs. PLUR -------------------------------------------------------
# # ----------------------------- ::ATTRB. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("ATTRIBUTE SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# aov1 <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = attrb))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$Plausibility), 2), " (", round( sd( attrb.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( attrb.plur$Plausibility), 2), " (", round( sd( attrb.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition, ds)
# print(means1)
# # ----------------------------- ::ASSOC. SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("PLAUSIBILITY SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# aov2 <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = assoc))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$Plausibility), 2), " (", round( sd( assoc.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$Plausibility), 2), " (", round( sd( assoc.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# # ----------------------------- ::UNRELATED SING vs PLUR------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("UNRELATED SING vs. PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov3 <- summary(aov(Plausibility ~ n2num + Error(item / n2num), data = unrel))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Singular","Plural", "PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( unrel.sing$Plausibility), 2), " (", round( sd( unrel.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Plausibility), 2), " (", round( sd( unrel.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: PLAUSIBILITY SING vs.  SING -------------------------------------------------------
# # ----------------------------- ::REL. SING vs UNREL. SING------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED SING vs. UNRELATED SING", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(Plausibility ~ related + Error(item / related ), data = attrb.unrel.sing))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.sing","Unrelated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$Plausibility), 2), " (", round( sd( attrb.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Plausibility), 2), " (", round( sd( unrel.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(Plausibility ~ related + Error(item / related ), data = attrb.assoc.sing))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.sing","Associated.sing","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.sing$Plausibility), 2), " (", round( sd( attrb.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( assoc.sing$Plausibility), 2), " (", round( sd( assoc.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(Plausibility ~ related + Error(item / related ), data = assoc.unrel.sing))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.sing","Unrelated.sing","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.sing$Plausibility), 2), " (", round( sd( assoc.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.sing$Plausibility), 2), " (", round( sd( unrel.sing$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# # --------------------- PROP: PLAUSIBILITY PLUR vs.  PLUR -------------------------------------------------------
# # ----------------------------- ::REL. PLUR vs UNREL. PLUR ------------
# cat(br, br, rep(c("="), times = 50, quote = F), br)
# cat("RELATED PLUR vs. UNRELATED PLUR", sep = "", fill = 80)
# cat(rep(c("="), times = 50, quote = F), br, br)
# 
# 
# aov1<- summary(aov(Plausibility ~ related + Error(item / related ), data = attrb.unrel.plur))
# p <- zapsmall(aov1[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Attributes.plur","Unrelated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$Plausibility), 2), " (", round( sd( attrb.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Plausibility), 2), " (", round( sd( unrel.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means1 <- cbind.data.frame(Condition,ds)
# print(means1)
# 
# ###
# 
# aov2 <- summary(aov(Plausibility ~ related + Error(item / related ), data = attrb.assoc.plur))
# p <- zapsmall(aov2[[2]][[1]][["Pr(>F)"]][1], digits=6)
# 
# Condition  = c("Attributes.plur","Associated.plur","PVal")
# ds = data.frame(
#   c(
#     paste( round( mean( attrb.plur$Plausibility), 2), " (", round( sd( attrb.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( assoc.plur$Plausibility), 2), " (", round( sd( assoc.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means2 <- cbind.data.frame(Condition, ds)
# print(means2)
# 
# ###
# 
# aov3 <- summary(aov(Plausibility ~ related + Error(item / related ), data = assoc.unrel.plur))
# p <- zapsmall(aov3[[2]][[1]][["Pr(>F)"]][1], digits=6)
# Condition  = c("Associated.plur","Unrelated.plur","PVal")
# 
# ds = data.frame(
#   c(
#     paste( round( mean( assoc.plur$Plausibility), 2), " (", round( sd( assoc.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( mean( unrel.plur$Plausibility), 2), " (", round( sd( unrel.plur$Plausibility), 2), ")", sep = ""),
#     paste( round( p, 3), get_stars(p))
#   ))
# colnames(ds)[1] <- "Total" 
# means3 <- cbind.data.frame(Condition, ds)
# print(means3)
# sink()
# closeAllConnections()
# 
