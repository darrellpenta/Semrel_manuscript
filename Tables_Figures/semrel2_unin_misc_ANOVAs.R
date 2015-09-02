
# ------------------------------------- CAT UNINFL F1------------------------------

source( file = "clear_and_setup.R")
source( file = "semrel2_CAT_error_data_read_in_dataframe.R")
a.2x2.f1 <- aov(unind ~ related * n2num + Error( subject / ( related * n2num)), data = d.cat)
# sink("output/table19_SR2_CAT_uninflected_f1_anova.txt")
cat("Table 19: CAT UNINFLECTED F1 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x2.f1), digits = 6)
sink()
aov.sum1 <- summary( a.2x2.f1)
View(ds)
#n2num
f.n2n <- zapsmall( aov.sum1[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum1[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum1[[3]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum1[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum1[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum1[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum1[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum1[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum1[[4]][[1]][["Mean Sq"]][2], digits = 4)

error.rate.ANOVA.1 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F1    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n),")", sep = ""),
    paste( m.rel," (", get_stars( p.rel),")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r),")", sep = ""))   
)


# ------------------------------------- CAT UNINFL F2------------------------------


source(file = "semrel2_CAT_uninflected_f2_read_in_dataframe.R")

a.2x2.f2 <- aov(unind ~ related * n2num + Error(item / (related * n2num)), data = d.cat)
sink("output/table19_SR2_CAT_uninflected_f2_anova.txt")
cat("Table 19: CAT UNINFLECTED F2 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x2.f2), digits = 6)
sink()

aov.sum2 <- summary( a.2x2.f2)

#n2num
f.n2n <- zapsmall( aov.sum2[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum2[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum2[[3]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum2[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum2[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum2[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum2[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum2[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum2[[4]][[1]][["Mean Sq"]][2], digits = 4)


error.rate.ANOVA.2 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F2    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n), ")", sep = ""),
    paste( m.rel," (", get_stars( p.rel), ")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r), ")", sep = ""))   
)


error.rate.ANOVA <- cbind(error.rate.ANOVA.1,error.rate.ANOVA.2[,c(2:3)])
View(error.rate.ANOVA)
write.xlsx(error.rate.ANOVA, file="output/table19_SR2_CAT__uninflected_anovas.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)

# ------------------------------------- PROP UNINFL F1 ------------------------------
source( file = "clear_and_setup.R")
source( file = "semrel2_PROP_error_data_read_in_dataframe.R")

a.2x3 <- aov(unind ~ related * n2num + Error( subject / ( related * n2num)), data = d.prop)
sink("output/table21_SR2_PROP_uninflected_f1_anova.txt")
cat("Table 21: PROP F1 UNINFLECTED 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x3), digits = 6)
sink()
aov.sum <- summary( a.2x3)

#n2num
f.n2n <- zapsmall( aov.sum[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum[[3]][[1]][["Mean Sq"]][2], digits = 4)
#rel
f.rel <- zapsmall( aov.sum[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum[[4]][[1]][["Mean Sq"]][2], digits = 4)

error.rate.ANOVA.1 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F1    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n),")", sep = ""),
    paste( m.rel," (", get_stars( p.rel),")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r),")", sep = ""))   
)

# ------------------------------------- PROP UNINFL F2 ------------------------------


source(file = "semrel2_CAT_uninflected_f2_read_in_dataframe.R")

a.2x3 <- aov(unind ~ related * n2num + Error(item / (related * n2num)), data = d.prop)
sink("output/table21_SR2_PROP_uninflected_f2_anova.txt")
cat("Table 21: PROP F2 UNINFLECTED 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep="", fill = 80)
print( summary( a.2x3), digits = 6)
sink()

aov.sum <- summary( a.2x3)

#n2num
f.n2n <- zapsmall( aov.sum[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum[[3]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum[[4]][[1]][["Mean Sq"]][2], digits = 4)


error.rate.ANOVA.2 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F2    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n), ")", sep = ""),
    paste( m.rel," (", get_stars( p.rel), ")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r), ")", sep = ""))   
)


error.rate.ANOVA <- cbind(error.rate.ANOVA.1,error.rate.ANOVA.2[,c(2:3)])

View(error.rate.ANOVA)
write.xlsx(error.rate.ANOVA, file="output/table21_SR2_PROP_uninflected_anovas.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)

# ------------------------------------- CAT MISC ------------------------------


source( file = "clear_and_setup.R")
source( file = "semrel2_CAT_error_data_read_in_dataframe.R")
a.2x2.f1 <- aov(misc ~ related * n2num + Error( subject / ( related * n2num)), data = d.cat)
sink("output/table20_SR2_CAT_miscellaneous_f1_anova.txt")
cat("Table 20: CAT MISCELLANEOUS F1 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x2.f1), digits = 6)
sink()
aov.sum1 <- summary( a.2x2.f1)

#n2num
f.n2n <- zapsmall( aov.sum1[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum1[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum1[[3]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum1[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum1[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum1[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum1[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum1[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum1[[4]][[1]][["Mean Sq"]][2], digits = 4)

error.rate.ANOVA.1 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F1    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n),")", sep = ""),
    paste( m.rel," (", get_stars( p.rel),")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r),")", sep = ""))   
)


# F2

source(file = "semrel2_CAT_uninflected_f2_read_in_dataframe.R")
a.2x2.f2 <- aov(error ~ related * n2num + Error(item / (related * n2num)), data = d.cat)
sink("output/table20_SR2_CAT_miscellaneous_f2_anova.txt")
cat("Table 20: CAT MISCELLANEOUS F2 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)

print( summary( a.2x2.f2), digits = 6)
sink()

aov.sum2 <- summary( a.2x2.f2)

#n2num
f.n2n <- zapsmall( aov.sum2[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum2[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum2[[3]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum2[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum2[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum2[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum2[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum2[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum2[[4]][[1]][["Mean Sq"]][2], digits = 4)


error.rate.ANOVA.2 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F2    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n), ")", sep = ""),
    paste( m.rel," (", get_stars( p.rel), ")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r), ")", sep = ""))   
)


error.rate.ANOVA <- cbind(error.rate.ANOVA.1,error.rate.ANOVA.2[,c(2:3)])
View(error.rate.ANOVA)
write.xlsx(error.rate.ANOVA, file="output/table20_SR2_CAT__miscellaneous_anovas.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)


# 
# ------------------------------------- PROP MISC ---------------------------
source( file = "clear_and_setup.R")
source( file = "semrel2_PROP_error_data_read_in_dataframe.R")

a.2x3 <- aov(misc ~ related * n2num + Error( subject / ( related * n2num)), data = d.prop)
sink("output/table22_SR2_PROP_miscellaneous_f1_anova.txt")
cat("Table 22: PROP F1 MISCELLANEOUS 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x3), digits = 6)
sink()
aov.sum <- summary( a.2x3)

#n2num
f.n2n <- zapsmall( aov.sum[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum[[3]][[1]][["Mean Sq"]][2], digits = 4)
#rel
f.rel <- zapsmall( aov.sum[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum[[4]][[1]][["Mean Sq"]][2], digits = 4)

error.rate.ANOVA.1 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F1    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n),")", sep = ""),
    paste( m.rel," (", get_stars( p.rel),")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r),")", sep = ""))   
)


source(file = "semrel2_CAT_uninflected_f2_read_in_dataframe.R")
a.2x3 <- aov(unind ~ related * n2num + Error(item / (related * n2num)), data = d.prop)
sink("output/table22_SR2_PROP_miscellaneous_f2_anova.txt")
cat("Table 22: PROP F2 MISCELLANEOUS 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep="", fill = 80)
print( summary( a.2x3), digits = 6)
sink()

aov.sum <- summary( a.2x3)

#n2num
f.n2n <- zapsmall( aov.sum[[3]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum[[3]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum[[2]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum[[4]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum[[4]][[1]][["Mean Sq"]][2], digits = 4)


error.rate.ANOVA.2 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness"
  ),
  
  F2    = c(
    f.n2n,
    f.rel,
    f.n2r
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n), ")", sep = ""),
    paste( m.rel," (", get_stars( p.rel), ")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r), ")", sep = ""))   
)


error.rate.ANOVA <- cbind(error.rate.ANOVA.1,error.rate.ANOVA.2[,c(2:3)])
View(error.rate.ANOVA)
write.xlsx(error.rate.ANOVA, file="output/table22_SR2_PROP_miscellaneous_anovas.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)


