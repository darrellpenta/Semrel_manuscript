# =============================================== SEMREL1 ========================================
# ================== SEMREL SUMMARY PARAGRAPH ============
source( file = "clear_and_setup.R")
source( file = "semrel_error_data_read_in_dataframe.R")

sink(file = "documents/SEMREL_results_section.txt")
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


main_effect1 <-cat("Participants produced more agreement errors in the plural local noun conditions than in the singular local noun conditions. Aditionally, more agreement errors were made for related preambles compared to unrelated preambles. Crucially, there was a two-way interaction bewteen local noun number and relatedness, and the head-local mismatch effect was larger for related cases (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,",\\textit{p}",get_range.tex( p),"; ", sep="" ) 
# ================== RELATED MISMATCH F2 ==========
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),") than for unrelated cases", sep="" )

# ================== UNRELATED MISMATCH F1 ========
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# ================== UNRELATED MISMATCH F2 ========
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),". There was no main effect of integration, nor a reliable interaciton between integration and any other factor.", sep="" )
cat(main_effect4)

# =============================================== SEMREL1 : ALTENRATIVE ANALYSES ========================
cat(br,br,line, br)
cat("===== RELATED x N2NUM  MISMATCH ALTERNATIVE ANALYSES",br)

# # ------------------ ARCSINE RELATED MISMATCH F1 -----------
# source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
# d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.relat)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: RELATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# # ------------------ ARCSINE RELATED MISMATCH F2 -----------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.relat)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"; UNRELATED", sep="" )
# 
# # ------------------ ARCSINE UNRELATED MISMATCH F1 ---------
# source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
# d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# # ------------------ ARCSINE UNRELATED MISMATCH F2 ---------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),".", sep="") 
# 
# cat(main_effect4,br)
# 
# # ------------------ ERR NO DYS RELATED MISMATCH F1 -----------
# source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
# d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.relat)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# 
# main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: RELATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# # ------------------ ERR NO DYS RELATED MISMATCH F2 -----------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.relat)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"; UNRELATED", sep="" )
# 
# # ------------------ ERR NO DYS UNRELATED MISMATCH F1 ---------
# source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
# d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# # ------------------ ERR NO DYS UNRELATED MISMATCH F2 ---------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),".", sep="") 
# cat(main_effect4,br)
# 
# # ------------------ ERR COUNTS RELATED MISMATCH F1 -----------
# source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
# d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.relat)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect1 <-paste("===== 3. ERRORS COUNTS: RELATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )  
# # ------------------ ERR COUNTS RELATED MISMATCH F2 -----------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.relat)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"; UNRELATED", sep="" )
# 
# # ------------------ ERR COUNTS UNRELATED MISMATCH F1 -----------
# source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
# d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# # ------------------ ERR COUNTS UNRELATED MISMATCH F2 -----------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),".", sep="") 
# cat(main_effect4)
# sink()


