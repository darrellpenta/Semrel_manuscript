#9/29 Changed LaTeX
# =============================================== SEMREL1 ========================================
sink(file = "documents/SEMREL_results_section.txt")


# ================== RELATEDNESS NORMING ============

source( file = "clear_and_setup.R")
source( file = "semrel_ratings_data_read_in_dataframe.R")
cat("RELATEDNESS NORMING PARA", br)
d0    <-  aov( RelatedHL ~ related * n2num + Error(item /related * n2num), data = d.sr)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

# (\Fval{1}{22}{7.71}, \MSe{2.33}\showp{, \p{05}})

rel.norm  <- paste("A 2 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of relatedness (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}","). There was no main effect of local noun number, nor an interaction between the two factors.", sep="" )
cat(rel.norm)
cat(br,line,br)
# ================== INTEGRATION NORMING ============

source( file = "clear_and_setup.R")
source( file = "semrel_ratings_data_read_in_dataframe.R")
cat("INTEG NORMING PARA", br)
d0    <-  aov( Integrated ~ (integrated + related + n2num)^3 + Error(item /(integrated + related + n2num)^3), data = d.sr)

d0.s <-summary(d0) 
dfr1.i <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2.i <- (d0.s[[2]][[1]][["Df"]][[2]])
f.i    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m.i    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p.i    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.r <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2.r <- (d0.s[[3]][[1]][["Df"]][[2]])
f.r    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m.r    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p.r    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)



int.norm1  <- paste( "A 2 (integration) $\\times$ 2 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of integration (\\Fval{", dfr1.i,"}{",dfr2.i,"}{",f.i,"}, \\MSe{",m.i,"}\\showp{, \\p{",get_range.tex(p.i),"}}","), and a main effect of relatedness (\\Fval{", dfr1.r,"}{",dfr2.r,"}{",f.r,"}, \\MSe{",m.r,"}\\showp{, \\p{",get_range.tex(p.r),"}}","). There was no interactions among any of the other factors.", sep = "")


d0   <- aov(Integrated ~ related + Error(item / related), data = d.sr)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)




int.norm2   <- paste(int.norm1, " Collapsing across local noun number, related preambles were rated as more integrated than unrelated preambles (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}","); ", sep = "")



d0   <- aov(Integrated ~ related + Error(item / related), data = integ.sing)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


int.norm3   <- paste(int.norm2, " pairwise comparisons revealed that integration ratings were larger for related items than for unrelated items in the integrated singular condition (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}","), ", sep = "")
                     
d0   <- aov(Integrated ~ related + Error(item / related), data = integ.plur)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)




int.norm4  <- paste(int.norm3, "the integrated plural condition (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}",")", sep = "")


d0   <- aov(Integrated ~ related + Error(item / related), data = unint.sing)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)



int.norm5  <- paste(int.norm4, " the unintegrated singular condition (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}",")", sep = "")

d0   <- aov(Integrated ~ related + Error(item / related), data = unint.plur)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

int.norm6  <- paste(int.norm5, " and the unintegrated plural condition (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}",")", sep = "")

cat(int.norm6)
cat(br,line,br)
# ================== ASSOCIATION NORMING ============
source( file = "clear_and_setup.R")
source( file = "semrel_ratings_data_read_in_dataframe.R")
cat("ASSOCIATION NORMING PARA", br)
d0    <-  aov( AssArc.H.L ~  related * n2num + Error(item /(related * n2num)), data = d.sr)

d0.s <-summary(d0) 
dfr1.r <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2.r <- (d0.s[[2]][[1]][["Df"]][[2]])
f.r    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m.r    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p.r    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.n <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2.n <- (d0.s[[3]][[1]][["Df"]][[2]])
f.n    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m.n    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p.n    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.rn <- (d0.s[[4]][[1]][["Df"]][[1]])
dfr2.rn <- (d0.s[[4]][[1]][["Df"]][[2]])
f.rn    <- zapsmall( d0.s[[4]][[1]][["F value"]][1], digits = 4)
m.rn    <- zapsmall( d0.s[[4]][[1]][["Mean Sq"]][2], digits = 4)
p.rn    <- zapsmall( d0.s[[4]][[1]][["Pr(>F)"]][1],  digits = 4)


ass.norm.1  <- paste( "The data were submitted to a 2 (relatedness) $\\times$ 2 (local noun number) ANOVA, which revealed a main effect of relatedness (\\Fval{", dfr1.r,"}{",dfr2.r,"}{",f.r,"}, \\MSe{",m.r,"}\\showp{, \\p{",get_range.tex(p.r),"}}","),  a main effect of local noun number (\\Fval{", dfr1.n,"}{",dfr2.n,"}{",f.n,"}, \\MSe{",m.n,"}\\showp{, \\p{",get_range.tex(p.n),"}}","), and a significant interaction between the relatedness and local noun number (\\Fval{", dfr1.rn,"}{",dfr2.rn,"}{",f.rn,"}, \\MSe{",m.rn,"}\\showp{, \\p{",get_range.tex(p.rn),"}}",").", sep = "")

d0    <-  aov( AssArc.H.L ~   n2num + Error(item / n2num), data = relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f   <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)





ass.norm.2 <-paste(ass.norm.1, " Additional analyses  confirmed that the main effect of local noun number, as well as the relatedness-local noun number interaction, were being driven by the difference in association in the related singular versus related plural conditions (\\Fval{", dfr1,"}{",dfr2,"}{",f,"}, \\MSe{",m,"}\\showp{, \\p{",get_range.tex(p),"}}","). Consistent with the intended manipulation, head nouns elicited their corresponding related local nouns and never elicited their corresponding unrelated local nouns.", sep = "")

cat(ass.norm.2)
cat(br,line,br)
# aov.1       <-  aov(Integrated ~ integrated * related + Error(item /integrated * related ), data = d.sr)
# 
# d0.s <-summary(aov.1) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# 
# d1.s <-summary(aov.2) 
# 
# dfr1.r <- (d1.s[[3]][[1]][["Df"]][[1]])
# dfr2.r <- (d1.s[[3]][[1]][["Df"]][[2]])
# f.r    <- zapsmall( d1.s[[3]][[1]][["F value"]][1], digits = 4)
# m.r    <- zapsmall( d1.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
# p.r    <- zapsmall( d1.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)



# ================== SEMREL SUMMARY PARAGRAPH ============
source( file = "clear_and_setup.R")
source( file = "semrel_error_data_read_in_dataframe.R")


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

#(\Fs[2]{104}{9.21}{.56}\showp{,\p{001}}; \Fi[2]{44}{6.59}{3.48}\showp{, \p{01}})

main_effect1 <-paste("Participants produced more agreement errors in the plural local noun conditions than in the singular local noun conditions. Additionally, more agreement errors were made for related preambles compared to unrelated preambles. Crucially, there was a two-way interaction between local noun number and relatedness, and the head-local mismatch effect was larger for related cases (\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" ) 

# ================== RELATED MISMATCH F2 ==========
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = relat)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)



main_effect2 <-paste(main_effect1,"\\Fi[", dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}",") than for unrelated cases", sep="" )

# ================== UNRELATED MISMATCH F1 ========
source(file = "semrel_related_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect3 <- paste(main_effect2, "(\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )

# ================== UNRELATED MISMATCH F2 ========
source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect4 <-paste(main_effect3,"\\Fi[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}",". There was no main effect of integration, though the direction of the effect was consistent with previous research \\cite<i.e.,>{solandpearl2004}, nor a reliable interaction between integration and any other factor.", sep="" )
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
# main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: RELATED (\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )     
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
# main_effect2 <-paste(main_effect1,"\\Fi[", dfr1,"]{",dfr2,"}{",f,"}{",m,", \\textit{p}", get_range.tex( p),"; UNRELATED", sep="" )
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
# main_effect3 <- paste(main_effect2, "(\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )
# # ------------------ ARCSINE UNRELATED MISMATCH F2 ---------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# main_effect4 <-paste(main_effect3,"\\Fi[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}",".", sep="") 
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
# main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: RELATED (\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )   
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
# main_effect2 <-paste(main_effect1,"\\Fi[", dfr1,"]{",dfr2,"}{",f,"}{",m,", \\textit{p}", get_range.tex( p),"; UNRELATED", sep="" )
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
# main_effect3 <- paste(main_effect2, "(\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )
# # ------------------ ERR NO DYS UNRELATED MISMATCH F2 ---------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# main_effect4 <-paste(main_effect3,"\\Fi[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}",".", sep="") 
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
# main_effect1 <-paste("===== 3. ERRORS COUNTS: RELATED (\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )  
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
# main_effect2 <-paste(main_effect1,"\\Fi[", dfr1,"]{",dfr2,"}{",f,"}{",m,", \\textit{p}", get_range.tex( p),"; UNRELATED", sep="" )
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
# main_effect3 <- paste(main_effect2, "(\\Fs[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}","; ", sep="" )
# # ------------------ ERR COUNTS UNRELATED MISMATCH F2 -----------
# source(file = "semrel_related_f2_ANOVAS_read_in_dataframe.R")
# d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.unrel)
# d0.s <-summary(d0) 
# dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
# dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
# f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
# m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
# p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
# main_effect4 <-paste(main_effect3,"\\Fi[",dfr1,"]{",dfr2,"}{",f,"}{",m,"}\\showp{, \\p{",get_range.tex( p),"}}",".", sep="") 
# cat(main_effect4)
 sink()


