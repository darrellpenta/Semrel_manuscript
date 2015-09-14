
# ============================================== SEMREL 2 Part 1==========================================


sink(file = "documents/SEMREL2_results_section.txt")
# ================== CAT. RELATEDNESS NORMING ============
source( file = "clear_and_setup.R")
source( file = "semrel2_CAT_ratings_data_read_in_dataframe.R")
cat("CAT: RELATEDNESS NORMING ")
d0    <-  aov( RelatedHL ~ related * n2num + Error(item /related * n2num), data = d.cat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
rel.norm  <- paste(
  "A 2 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"). There was no main effect of local noun number, nor an interaction between the two factors.", sep="" )

cat(rel.norm)

# ================== CAT. INTEGRATION NORMING ============
source( file = "clear_and_setup.R")
source( file = "semrel2_CAT_ratings_data_read_in_dataframe.R")
cat("CAT: RELATEDNESS NORMING PARA")
d0    <-  aov( Integrated ~ related * n2num + Error(item /related * n2num), data = d.cat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
int.norm  <- paste(
  "A 2 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"). There was no main effect of local noun number, nor an interaction between the two factors.", sep="" )
cat(int.norm)

# ================== PROP. RELATEDNESS NORMING ============
source( file = "clear_and_setup.R")
source( file = "semrel2_PROP_ratings_data_read_in_dataframe.R")
cat("PROP: RELATEDNESS NORMING ")
d0    <-  aov( RelatedHL ~ related * n2num + Error(item /related * n2num), data = d.prop)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.rn <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2.rn <- (d0.s[[5]][[1]][["Df"]][[2]])
f.rn    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m.rn    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p.rn    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm1  <- paste("A 3 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"), as well as a marginally significant interaction between relatedness and local noun number (\\textit{F}(", dfr1.rn,",",dfr2.rn,")=",f,", \\textit{MS$_e$}=",m.rn,", \\textit{p}", get_range.tex(p.rn),").", sep="" )
########
d0    <-  aov( RelatedHL ~ related * n2num + Error(item /related * n2num), data = attrb.assoc)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm2  <- paste(rel.norm1, "A comparison of attribute associates and pure associates showed a signficant main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"), with attribute associates receiving higher relatedness ratings than associates", sep="" )

# cat(line,br)
# cat("ATTRIBUTES & ASSOCIATES", br)
# print(d0.s)

d0    <-  aov( RelatedHL ~ related * n2num + Error(item /related * n2num), data = assoc.unrel)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm3  <- paste(rel.norm2, "Pure associates were also rated as significantly more related than the unrelated control items (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"). ", sep="" )

# cat(line,br)
# cat("ASSOCIATES & UNRELATED", br)
# print(d0.s)


d0    <-  aov( RelatedHL ~ related * n2num + Error(item /related * n2num), data = attrb.unrel)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm4  <- paste(rel.norm3, "Attribute associates were also rated as significantly more related than the unrelated control items (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"). ", sep="" )

# cat(line,br)
# cat("ATTRIBUTES & UNRELATED", br)
# print(d0.s)


d0   <- aov(RelatedHL ~ n2num + Error(item / n2num), data = assoc.unrel)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm5   <- paste(rel.norm4, " Analyses of singular vs. plural conditions for each combination of the levels of relatedness revealed a marginally significant effect of local noun number when associates were compared with unrelated items (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),".", sep = "")


# cat(line,br)
# cat("ASSOCIATES & UNRELATED: N2NUM", br)
# print(d0.s)


d0   <- aov(RelatedHL ~ related + Error(item / related), data = assoc.unrel.sing)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm6   <- paste(rel.norm5, ": Relatedness ratings for singular associates were significantly larger than for singular unrelated items  (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),",", sep = "")

# cat(line,br)
# cat("ASSOCIATES & UNRELATED: SINGULAR", br)
# print(d0.s)


d0   <- aov(RelatedHL ~ related + Error(item / related), data = assoc.unrel.plur)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

rel.norm7   <- paste(rel.norm6, "and ratings for plural associates were significantly larger than for plural unrelated items  (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),".", sep = "")

# cat(line,br)
# cat("ASSOCIATES & UNRELATED: PLURAL", br)
# print(d0.s)
cat(rel.norm6)




# ================== PROP. INTEGRATION NORMING ============
source( file = "clear_and_setup.R")
source( file = "semrel2_PROP_ratings_data_read_in_dataframe.R")
cat("PROP: Integration NORMING")
d0    <-  aov( Integrated ~ related * n2num + Error(item /related * n2num), data = d.prop)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

int.norm1  <- paste("A 3 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),");", sep="" )

# cat(line,br)
# cat("FULL 3X2")
# print(d0.s)

d0    <-  aov( Integrated ~ related * n2num + Error(item /related * n2num), data = attrb.assoc)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

int.norm2  <- paste(int.norm1, "A comparison of Attribute and Associates showed a signficant main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"), with attribute ratings higher than associates", sep="" )

# cat(line,br)
# cat("ATTRIBUTES & ASSOCIATES", br)
# print(d0.s)

d0    <-  aov( Integrated ~ related * n2num + Error(item /related * n2num), data = assoc.unrel)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

int.norm3  <- paste(int.norm2, " Associates were also rated higher than the unrelated items (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"), ", sep="" )

# cat(line,br)
# cat("ASSOCIATES & UNRELATED", br)
# print(d0.s)

d0    <-  aov( Integrated ~ related * n2num + Error(item /related * n2num), data = attrb.unrel)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

int.norm4  <- paste(int.norm3, " as were attributes  (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),"), ", sep="" )

# cat(line,br)
# cat("ATTRIBUTES & UNRELATED",br)
# print(d0.s)
# cat(line, br)
cat(int.norm4)


# ================== PROP. ASSOCIATION NORMING ============
source( file = "clear_and_setup.R")
source( file = "semrel2_PROP_ratings_data_read_in_dataframe.R")
cat("PROP: ASSOCIATION NORMING ")
d0    <-  aov( AssArc.H.L ~ related * n2num + Error(item /related * n2num), data = d.prop)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.an <-          (d0.s[[5]][[1]][["Df"]][[1]])
dfr2.an <-          (d0.s[[5]][[1]][["Df"]][[2]])
f.an    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m.an    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p.an    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

ass.norm1  <- paste("A 3 (relatedness) $\\times$ 2 (local noun number) ANOVA on the data revealed a main effect of relatedness (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),") and a marginally significant interaction between relatedness and local noun number (\\textit{F}(", dfr1.an,",",dfr2.an,")=",f.an,", \\textit{MS$_e$}=",m.an,", \\textit{p}", get_range.tex(p.an),").", sep="")

# cat(line,br)
# cat("FULL 3X2", br)
# print(d0.s)

d0    <-  aov( AssArc.H.L ~ related * n2num + Error(item /related * n2num), data = attrb.assoc)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

ass.norm2  <- paste(ass.norm1, "Proportions were significantly greater for attribute associates than for pure associates, evidenced by a main effect of relatedness when the subset ofattribute associates and pure associates were analyzed (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),");", sep="")

# cat(line,br)
# cat("ATTRB & ASSOC", br)
# print(d0.s)

d0    <-  aov( AssArc.H.L ~ related * n2num + Error(item /related * n2num), data = attrb.unrel)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.rn <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2.rn <- (d0.s[[5]][[1]][["Df"]][[2]])
f.rn    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m.rn    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p.rn    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)



ass.norm3  <- paste(ass.norm2, "Comparing the subset of attribute associates with unrelated items also revealed a main effect of relatedness, with attribute associates generating higher association proportions than the unrelated items, (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p},", get_range.tex(p),") as well as a marginally significant interaction between relatedness and local noun number  (\\textit{F}(", dfr1.rn,",",dfr2.rn,")=",f.rn,", \\textit{MS$_e$}=",m.rn,", \\textit{p}", get_range.tex(p.rn),").", sep="")

# cat(line,br)
# cat("ATTRB & UNREL", br)
# print(d0.s)


d0   <- aov(AssArc.H.L ~ related + Error(item / related), data = attrb.unrel.sing)
d1   <- aov(AssArc.H.L ~ related + Error(item / related), data = attrb.unrel.plur)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

d1.s  <- summary(d1)
dfr1.p <-          (d1.s[[2]][[1]][["Df"]][[1]])
dfr2.p <-          (d1.s[[2]][[1]][["Df"]][[2]])
f.p    <- zapsmall( d1.s[[2]][[1]][["F value"]][1], digits = 4)
m.p    <- zapsmall( d1.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p.p    <- zapsmall( d1.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)



ass.norm4  <- paste(ass.norm3, "The interaction in this case is due to the fact that association proportions for singular attribute associates were significantly larger than for singular unrelated items  (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),", and proportions for plural attribute associates were also larger than for the unrelated items (\\textit{F}(", dfr1.p,",",dfr2.p,")=",f.p,", \\textit{MS$_e$}=",m.p,", \\textit{p}", get_range.tex(p.p),".", sep = "")

# cat(line,br)
# cat("ATTRIBUTE & UNRELATED SING", br)
# print(d0.s)

# cat(line,br)
# cat("ATTRIBUTE & UNRELATED PLUR", br)
# print(d1.s)



d0    <-  aov( AssArc.H.L ~ related * n2num + Error(item /related * n2num), data = assoc.unrel)
d0.s <-summary(d0)

dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

dfr1.rn <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2.rn <- (d0.s[[5]][[1]][["Df"]][[2]])
f.rn    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m.rn    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p.rn    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)



ass.norm5  <- paste(ass.norm4, "A comparison of pure assocciates and unrelated items also revealed a main effect of relatedness, with pure associates generating higher proportions than the unrelated items, (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex(p),") as well as a significant interaction between relatedness and local noun number  (\\textit{F}(", dfr1.rn,",",dfr2.rn,")=",f.rn,", \\textit{MS$_e$}=",m.rn,", \\textit{p}", get_range.tex(p.rn),").", sep="")
           
# cat(line,br)
# cat("ASSOC & UNREL", br)
# print(d0.s)


d0   <- aov(AssArc.H.L ~ related + Error(item / related), data = assoc.unrel.sing)
d1   <- aov(AssArc.H.L ~ related + Error(item / related), data = assoc.unrel.plur)
d0.s  <- summary(d0)
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)



ass.norm6  <- paste(ass.norm5, "In this case, the interaction iwas being driven by a significant different in proportions for the singular  associates relative to the singular unrelated items  (\\textit{F}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex(p),".", sep = "")

# cat(line,br)
# cat("ASSOC & UNRELATED SING", br)
# print(d0.s)

# cat(line,br)
# cat("ASSOC & UNRELATED PLUR", br)
# print(d1.s)

cat(ass.norm6)



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

main_effect1 <-paste("Comparing attributes and associates, there was a main effect of relationship, with associates generating more errors than attributes (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ================== RELATEDNESS MAIN EFFECT F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),").", sep="" )

# ================== RELATEDNESS X N2NUM  F1 ==============

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R") 
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <-paste(main_effect2, "There was also a marginally significant interaction between relatedness and local noun number  (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ================== RELATEDNESS X N2NUM  F2 ==============

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"), the mismatch effect being larger for associates (\\textit{F$_1$}(", sep="" )


# ================== ASSOCIATES MISMATCH F1 ==============
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect5 <-paste(main_effect4, dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),";", sep="")
# ================== ASSOCIATES MISMATCH F1 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect6 <-paste(main_effect5,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),") than for attributes (\\textit{F$_1$}(", sep="" )

# ================== ATTRIBUTES MISMATCH F1 ==============
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect7 <-paste(main_effect6, dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# ================== ATTRIBUTES MISMATCH F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect8 <-paste(main_effect7,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),").", sep="" )
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


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ARCSINE RELATEDNESS ME F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("===== 3. ERROR COUNTS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )
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


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
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

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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


main_effect1 <-paste("===== 3. ERROR COUNTS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATED X N2NUM F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )
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

main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: ASSOCIATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ARCSINE ASSOCIATES MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"; ATTRIBUTE", sep="" )

# ------------------ ARCSINE ATTRIBUTES MISMATCH F1 ---------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(subj / n2num), data = arc.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# ------------------ ARCSINE ATTRIBUTES MISMATCH F2 ---------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),".", sep="") 

cat(main_effect4, br)

# ------------------ ERR NO DYS ASSOCIATED MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.assoc)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: ASSOCIATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR NO DYS ASSOCIATED MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"; ATTRIBUTE", sep="" )

# ------------------ ERR NO DYS ATTRIBUTE MISMATCH F1 ---------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# ------------------ ERR NO DYS ATTRIBUTE MISMATCH F2 ---------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),".", sep="") 
cat(main_effect4, br)

# ------------------ ERR COUNTS ASSOCIATE MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.assoc)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERRORS COUNTS: ASSOCIATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )  
# ------------------ ERR COUNTS ASSOCIATE MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.assoc)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"; ATTRIBUTE", sep="" )

# ------------------ ERR COUNTS ATTRIBUTE MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <- paste(main_effect2, "(\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# ------------------ ERR COUNTS ATTRIBUTE MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.relat)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),".", sep="") 
cat(main_effect4, br)

# ============================================== SEMREL 2 Part 2==========================================
source( file = "clear_and_setup.R")
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

main_effect1 <-paste("Associates also generated more agreement errors than unrelated items (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ================== ASSOCIATE vs. UNRELATED MAIN EFFECT F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"),", sep="" )

# ================== ASSOCIATE X N2NUM  F1 ==============

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <-paste(main_effect2, "and there was a local noun number by associate interaction (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ================== ASSOCIATE X N2NUM  F2 ==============

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"), the mismatch effect being larger for associates than for unrelated items (\\textit{F$_1$}(", sep="" )

# ================== UNRELATED MISMATCH F1 ==============
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(subj / n2num), data = unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect5 <-paste(main_effect4, dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )
# ================== UNRELATED MISMATCH F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ n2num + Error(item / n2num), data = unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect6 <-paste(main_effect5,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),").", sep="" )
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


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ARCSINE RELATEDNESS ME F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("===== 3. ERROR COUNTS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )
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


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
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

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.assoc.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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


main_effect1 <-paste("===== 3. ERROR COUNTS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATED X N2NUM F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )
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

main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: UNRELATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ARCSINE UNRELATED MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ n2num + Error(item / n2num), data = arc.unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR NO DYS UNRELATED MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(subj / n2num), data = nodys.unrel)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: UNRELATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR NO DYS UNRELATED MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ n2num + Error(item / n2num), data = nodys.unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

cat(main_effect2, br)

# ------------------ ERR COUNTS UNRELATED MISMATCH F1 -----------
source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(subj / n2num), data = cnt.unrel)

dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect1 <-paste("===== 3. ERRORS COUNTS: UNRELATED (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )  
# ------------------ ERR COUNTS ASSOCIATE MISMATCH F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ n2num + Error(item / n2num), data = cnt.unrel)

d0.s <-summary(d0) 
dfr1 <- (d0.s[[2]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[2]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[2]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[2]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[2]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("A comparison of attribute and unrelated items revealed a marginal main effect of relationship, as related items generated more errors than unrelated items (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ================== ATTRIBUTE vs. UNRELATED MAIN EFFECT F2 ==============
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),").", sep="" )

# ================== ATTRIBUTE X N2NUM  F1 ==============

source(file = "semrel2_PROP_f1_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(subj / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 

dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect3 <-paste(main_effect2, "There was also an interaction that was significant by items and marginally significant by subjects (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ================== ATTRIBUTE X N2NUM  F2 ==============

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(error ~ related * n2num + Error(item / related * n2num), data = relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect4 <-paste(main_effect3,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p),"), the mismatch effect being larger for attributes than for unrelated items.", sep="")

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


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ARCSINE RELATEDNESS ME F2 -----------

source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(arcerr ~ related * n2num + Error(item / related * n2num), data = arc.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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

main_effect1 <-paste("===== 3. ERROR COUNTS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATEDNESS ME F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[3]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[3]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[3]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[3]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[3]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )
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


main_effect1 <-paste("===== 1. ARCSIN TRANSFORMED PROPORTIONS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
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

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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


main_effect1 <-paste("===== 2. ERRORS NO DYSFLUENCIES: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )     
# ------------------ ERR NO DYS RELATED X N2NUM F1 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(nodys ~ related * n2num + Error(item / related * n2num), data = nodys.relat.unrel)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )

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


main_effect1 <-paste("===== 3. ERROR COUNTS: (\\textit{F$_1$}(",dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}",get_range.tex( p),"; ", sep="" )   
# ------------------ ERR COUNTS RELATED X N2NUM F2 -----------
source(file = "semrel2_PROP_f2_ANOVAS_read_in_dataframe.R")
d0   <- aov(count ~ related * n2num + Error(item / related * n2num), data = cnt.relat.assoc)
d0.s <-summary(d0) 
dfr1 <- (d0.s[[5]][[1]][["Df"]][[1]])
dfr2 <- (d0.s[[5]][[1]][["Df"]][[2]])
f    <- zapsmall( d0.s[[5]][[1]][["F value"]][1], digits = 4)
m    <- zapsmall( d0.s[[5]][[1]][["Mean Sq"]][2], digits = 4)
p    <- zapsmall( d0.s[[5]][[1]][["Pr(>F)"]][1],  digits = 4)

main_effect2 <-paste(main_effect1,"\\textit{F$_2$}(", dfr1,",",dfr2,")=",f,", \\textit{MS$_e$}=",m,", \\textit{p}", get_range.tex( p), sep="" )
cat(main_effect2, br)

sink()

