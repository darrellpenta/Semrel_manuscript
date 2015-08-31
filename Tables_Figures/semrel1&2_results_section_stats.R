# =============================================== SEMREL1 ========================================
source( file = "clear_and_setup.R")
source( file = "semrel_error_data_read_in_dataframe.R")

sink(file = "documents/results_section.txt")
paste("SEMREL RESULTS")
cat(line,line,br)
summary <- paste("Accross all ",length( d.sr$maincode)," trials, there were ", sum( d.sr$corrd), " correctly inflected respones, ", sum( d.sr$errd), " agreement errors, ", sum( d.sr$unind), " uninflected responses, ", sum( d.sr$misc), " miscellaneous cases, and ",sum( d.sr$noresp), " trials with no response.", sep="")
print(summary)
cat( br, br, line, br)


source( file = "clear_and_setup.R")
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("Participants produced more agreement errors in the plural local noun conditions than in the singular local noun conditions. Aditionally, more agreement errors were made for realted preambles versus unrelated preambles. Crucially, there was a two-way interaction bewteen local noun number and relatedness, and the head-local mismatch effect was larger for related cases (F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )     
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"F2(", dfr1,",",dfr2,")=",f,", MSe=",m,",p", get_range( p),") than for unrelated cases", sep="" )

source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )

source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"F2(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),". There was no main effect of integration, nor a reliable interaciton between integration and any other factor.", sep="" )
print(main_effect4)

# ---------------------------- ARCSINE
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("ARCSIN TRANSFORMED PROPORTIONS: RELATED (F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )     
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"F2(", dfr1,",",dfr2,")=",f,", MSe=",m,",p", get_range( p),"; UNRELATED", sep="" )

source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )

source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"F2(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),".", sep="") 
cat(br,br, line,br)
print(main_effect4)

# ---------------------------- ERRORS NO DYS
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("ERRORS NO DYSFLUENCIES: RELATED (F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )     
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"F2(", dfr1,",",dfr2,")=",f,", MSe=",m,",p", get_range( p),"; UNRELATED", sep="" )

source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )

source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = ndys.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"F2(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),".", sep="") 
cat(br,br, line,br)
print(main_effect4)

# ---------------------------- ERROR COUNTS
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("ERRORS COUNTS: RELATED (F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )     
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"F2(", dfr1,",",dfr2,")=",f,", MSe=",m,",p", get_range( p),"; UNRELATED", sep="" )

source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )

source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"F2(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),".", sep="") 
cat(br,br, line,br)
print(main_effect4)


# ============================================== SEMREL 2 ==========================================
source( file = "clear_and_setup.R")
source( file = "semrel2_ALL_error_data_read_in_dataframe.R")

cat(br,br,br,line,line, line,br)

paste("SEMREL 2 RESULTS")
cat(line,line,br)
summary <- paste("Accross all ",length( d.sr2$maincode)," trials, there were ", sum( d.sr2$corrd), " correctly inflected respones, ", sum( d.sr2$errd), " agreement errors, ", sum( d.sr2$unind), " uninflected responses, ", sum( d.sr2$misc), " miscellaneous cases, and ",sum( d.sr2$noresp), " trials with no response.", sep="")
print(summary)
cat( br, br, line, br)


source( file = "clear_and_setup.R")
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("Comparing attributes and associates, there was a main effect of relationship, with associates generating more errors than attributes (F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )     

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"F2(", dfr1,",",dfr2,")=",f,", MSe=",m,",p", get_range( p),").", sep="" )

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect3 <-paste(main_effect2, "There was also a marginally significant interaction between relatedness and local noun number  (F1(",dfr1,",",dfr2,")=",f,", MSe=",m,",p",get_range( p),"; ", sep="" )     

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"F2(", dfr1,",",dfr2,")=",f,", MSe=",m,",p", get_range( p),").", sep="" )
main_effect4


sink()