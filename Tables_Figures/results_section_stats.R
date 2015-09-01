
# F1=  
# F2=  \textit{F$_2$}

# =============================================== SEMREL1 ========================================
# ================== SUMMARY PARAGRAPH ============
source( file = "clear_and_setup.R")
source( file = "semrel_error_data_read_in_dataframe.R")

sink(file = "documents/results_section.txt")
cat("SEMREL RESULTS",br)
cat(line,br,line,br)
summary <- paste("Accross all ",length( d.sr$maincode)," trials, there were ", sum( d.sr$corrd), " correctly inflected respones, ", sum( d.sr$errd), " agreement errors, ", sum( d.sr$unind), " uninflected responses, ", sum( d.sr$misc), " miscellaneous cases, and ",sum( d.sr$noresp), " trials with no response.", sep="")
cat(summary, br, br, line, br)


# ================== RELATED MISMATCH F1 ==========
source( file = "clear_and_setup.R")
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-cat("Participants produced more agreement errors in the plural local noun conditions than in the singular local noun conditions. Aditionally, more agreement errors were made for realted preambles versus unrelated preambles. Crucially, there was a two-way interaction bewteen local noun number and relatedness, and the head-local mismatch effect was larger for related cases (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,",\textit{p}",get_range( p),"; ", sep="" ) 
# ================== RELATED MISMATCH F2 ==========
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,",\textit{p}", get_range( p),") than for unrelated cases", sep="" )

# ================== UNRELATED MISMATCH F1 ========
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,",\textit{p}",get_range( p),"; ", sep="" )
# ================== UNRELATED MISMATCH F2 ========
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,",\textit{p}",get_range( p),". There was no main effect of integration, nor a reliable interaciton between integration and any other factor.", sep="" )
cat(main_effect4)

# =============================================== SEMREL1 : ALTENRATIVE ANALYSES ========================
cat(br,br,line, br)
cat("===== RELATED x N2NUM  MISMATCH ALTERNATIVE ANALYSES",br)

# ------------------ ARCSINE RELATED MISMATCH F1 -----------
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: RELATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATED MISMATCH F2 -----------
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"; UNRELATED", sep="" )

# ------------------ ARCSINE UNRELATED MISMATCH F1 ---------
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ------------------ ARCSINE UNRELATED MISMATCH F1 ---------
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),".", sep="") 

cat(main_effect4,br)

# ------------------ ERR NO DYS RELATED MISMATCH F1 -----------
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: RELATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR NO DYS RELATED MISMATCH F2 -----------
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"; UNRELATED", sep="" )

# ------------------ ERR NO DYS UNRELATED MISMATCH F1 ---------
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ------------------ ERR NO DYS UNRELATED MISMATCH F2 ---------
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = ndys.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),".", sep="") 
cat(main_effect4,br)

# ------------------ ERR COUNTS RELATED MISMATCH F1 -----------
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERRORS COUNTS: RELATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )  
# ------------------ ERR COUNTS RELATED MISMATCH F2 -----------
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"; UNRELATED", sep="" )

# ------------------ ERR COUNTS UNRELATED MISMATCH F1 -----------
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ------------------ ERR COUNTS UNRELATED MISMATCH F2 -----------
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),".", sep="") 
cat(main_effect4)


# ============================================== SEMREL 2 Part 1==========================================
source( file = "clear_and_setup.R")
source( file = "semrel2_ALL_error_data_read_in_dataframe.R")

cat(br,br,br,br)

# ================== SUMMARY PARAGRAPH ============
cat("SEMREL 2 RESULTS",br)
cat(line,br, line, br)
summary <- paste("Accross all ",length( d.sr2$maincode)," trials, there were ", sum( d.sr2$corrd), " correctly inflected respones, ", sum( d.sr2$errd), " agreement errors, ", sum( d.sr2$unind), " uninflected responses, ", sum( d.sr2$misc), " miscellaneous cases, and ",sum( d.sr2$noresp), " trials with no response.", sep="")
cat(summary)
cat( br, br, line, br)

# ================== RELATEDNESS MAIN EFFECT F1 ==============
source( file = "clear_and_setup.R")
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("Comparing attributes and associates, there was a main effect of relationship, with associates generating more errors than attributes (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ================== RELATEDNESS MAIN EFFECT F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),").", sep="" )

# ================== RELATEDNESS X N2NUM  F1 ==============

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <-paste(main_effect2, "There was also a marginally significant interaction between relatedness and local noun number  (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ================== RELATEDNESS X N2NUM  F2 ==============

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"), the mismatch effect being larger for associates (\textit{F$_1$}(", sep="" )


# ================== ASSOCIATES MISMATCH F1 ==============
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect5 <-paste(main_effect4, dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),";", sep="")
# ================== ASSOCIATES MISMATCH F1 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect6 <-paste(main_effect5,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),") than for attributes (\textit{F$_1$}(", sep="" )

# ================== ATTRIBUTES MISMATCH F1 ==============
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect7 <-paste(main_effect6, dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ================== ATTRIBUTES MISMATCH F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect8 <-paste(main_effect7,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),").", sep="" )
cat(main_effect8,br)

# =============================================== SEMREL2 : ALTENRATIVE ANALYSES Part 1========================

# ------------------ ARCSINE RELATEDNESS ME F1 -----------

cat(br,br,line,br)
cat("===== MAIN EFFECT OF RELATEDNESS", br)
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(subj / related * n2num), data = arc.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATEDNESS ME F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2,br)

# ------------------ ERR NO DYS RELATEDNESS ME F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(subj / related * n2num), data = nodys.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2,br)

# ------------------ ERR COUNTS RELATEDNESS ME F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(subj / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERROR COUNTS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )
cat(main_effect2,br)


# ------------------ ARCSINE RELATED X N2NUM F1 -----------

cat(br,br,line,br)
cat("===== RELATEDNESS X N2NUM ", br)
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")

d0   <- aov(arcerr ~ related * n2num + Error(subj / related * n2num), data = arc.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATED X N2NUM F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
# ---------------------------- ARCSINE
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(subj / related * n2num), data = nodys.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR COUNTS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(subj / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 3. ERROR COUNTS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATED X N2NUM F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )
cat(main_effect2, br)


# ------------------ ARCSINE ASSOCIATES MISMATCH F1 -----------
cat(br,br,line,br)
cat("===== ASSOCIATES X ATTRIBUTES MISMATCH EFFECTS", br)

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: ASSOCIATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE ASSOCIATES MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"; ATTRIBUTE", sep="" )

# ------------------ ARCSINE ATTRIBUTES MISMATCH F1 ---------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ------------------ ARCSINE ATTRIBUTES MISMATCH F2 ---------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),".", sep="") 

cat(main_effect4, br)

# ------------------ ERR NO DYS ASSOCIATED MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.assoc)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: ASSOCIATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR NO DYS ASSOCIATED MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"; ATTRIBUTE", sep="" )

# ------------------ ERR NO DYS ATTRIBUTE MISMATCH F1 ---------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ------------------ ERR NO DYS ATTRIBUTE MISMATCH F2 ---------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),".", sep="") 
cat(main_effect4, br)

# ------------------ ERR COUNTS ASSOCIATE MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.assoc)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERRORS COUNTS: ASSOCIATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )  
# ------------------ ERR COUNTS ASSOCIATE MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"; ATTRIBUTE", sep="" )

# ------------------ ERR COUNTS ATTRIBUTE MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ------------------ ERR COUNTS ATTRIBUTE MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),".", sep="") 
cat(main_effect4, br)

# ============================================== SEMREL 2 Part 2==========================================
source( file = "clear_and_setup.R")
source( file = "semrel2_ALL_error_data_read_in_dataframe.R")

cat(br,br,br,line,br)

# ================== ASSOCIATE vs. UNRELATED MAIN EFFECT F1 ==============
source( file = "clear_and_setup.R")
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("Associates also generated more agreement errors than unrelated items (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ================== ASSOCIATE vs. UNRELATED MAIN EFFECT F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"),", sep="" )

# ================== ASSOCIATE X N2NUM  F1 ==============

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <-paste(main_effect2, "and there was a local noun number by associate interaction (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ================== ASSOCIATE X N2NUM  F2 ==============

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"), the mismatch effect being larger for associates than for unrelated items (\textit{F$_1$}(", sep="" )

# ================== UNRELATED MISMATCH F1 ==============
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect5 <-paste(main_effect4, dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )
# ================== UNRELATED MISMATCH F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect6 <-paste(main_effect5,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),").", sep="" )
cat(main_effect6, br)

# ============================================== SEMREL 2 ALTERNATIVE ANALYSES Part 2 ================
# ------------------ ARCSINE RELATEDNESS ME F1 -----------

cat(br,br,line,br)
cat("===== MAIN EFFECT OF RELATEDNESS", br)
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(subj / related * n2num), data = arc.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATEDNESS ME F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2,br)

# ------------------ ERR NO DYS RELATEDNESS ME F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(subj / related * n2num), data = nodys.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2,br)

# ------------------ ERR COUNTS RELATEDNESS ME F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(subj / related * n2num), data = cnt.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERROR COUNTS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )
cat(main_effect2,br)

# ------------------ ARCSINE RELATED X N2NUM F1 -----------

cat(br,br,line,br)
cat("===== RELATEDNESS X N2NUM ", br)
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")

d0   <- aov(arcerr ~ related * n2num + Error(subj / related * n2num), data = arc.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATED X N2NUM F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
# ---------------------------- ARCSINE
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(subj / related * n2num), data = nodys.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR COUNTS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(subj / related * n2num), data = cnt.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 3. ERROR COUNTS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATED X N2NUM F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )
cat(main_effect2, br)

# ------------------ ARCSINE UNRELATED MISMATCH F1 -----------
cat(br,br,line,br)
cat("===== ASSOCIATES X UNRELEATED MISMATCH EFFECTS", br)

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: UNRELATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE UNRELATED MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR NO DYS UNRELATED MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.unrel)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: UNRELATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR NO DYS UNRELATED MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.unrelated)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR COUNTS UNRELATED MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.unrel)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERRORS COUNTS: UNRELATED (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )  
# ------------------ ERR COUNTS ASSOCIATE MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ============================================== SEMREL 2 Part 3==========================================
source( file = "clear_and_setup.R")
source( file = "semrel2_ALL_error_data_read_in_dataframe.R")

cat(br,br,br,line,br)

# ================== ATTRIBUTE vs. UNRELATED MAIN EFFECT F1 ==============
source( file = "clear_and_setup.R")
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("A comparison of attribute and unrelated items revealed a marginal main effect of relationship as related items generated more errors than unrelated items (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ================== ATTRIBUTE vs. UNRELATED MAIN EFFECT F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),").", sep="" )

# ================== ATTRIBUTE X N2NUM  F1 ==============

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <-paste(main_effect2, "There was also an interaction that was significant by items and marginally significant by subjects (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ================== ATTRIBUTE X N2NUM  F2 ==============

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p),"), the mismatch effect being larger for attributes than for unrelated items.", sep="")

cat(main_effect4, br)

# ============================================== SEMREL 2 ALTERNATIVE ANALYSES Part 3 ================
# ------------------ ARCSINE RELATEDNESS ME F1 -----------

cat(br,br,line,br)
cat("===== MAIN EFFECT OF RELATEDNESS", br)
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(subj / related * n2num), data = arc.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATEDNESS ME F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2,br)

# ------------------ ERR NO DYS RELATEDNESS ME F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(subj / related * n2num), data = nodys.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2,br)

# ------------------ ERR COUNTS RELATEDNESS ME F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(subj / related * n2num), data = cnt.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERROR COUNTS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )
cat(main_effect2,br)

# ------------------ ARCSINE RELATED X N2NUM F1 -----------

cat(br,br,line,br)
cat("===== RELATEDNESS X N2NUM ", br)
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")

d0   <- aov(arcerr ~ related * n2num + Error(subj / related * n2num), data = arc.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ARCSINE RELATED X N2NUM F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
# ---------------------------- ARCSINE
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(subj / related * n2num), data = nodys.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR COUNTS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(subj / related * n2num), data = cnt.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 3. ERROR COUNTS: (\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}",get_range( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATED X N2NUM F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \textit{MS$_e$}=",m,", \textit{p}", get_range( p), sep="" )
cat(main_effect2, br)


