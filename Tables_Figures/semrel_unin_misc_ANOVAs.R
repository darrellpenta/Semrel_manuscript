
# =============================================== SEMREL1 ========================================

source( file = "clear_and_setup.R")
source( file = "semrel_error_data_read_in_dataframe.R")

a.2x2x2.f1 <- aov(unind ~ integrated * related * n2num + Error( subject / ( integrated * related * n2num)), data = d.sr)

sink("output/table17_SR_uninflected_f1_anova.txt")
cat("Table 17: F1 Uninflected 2 x 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x2x2.f1), digits = 6)
sink()
aov.sum <- summary( a.2x2x2.f1)

#n2num
f.n2n <- zapsmall( aov.sum[[4]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum[[4]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum[[3]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum[[3]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum[[7]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum[[7]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum[[7]][[1]][["Mean Sq"]][2], digits = 4)
#semint
f.int <- zapsmall( aov.sum[[2]][[1]][["F value"]][1], digits = 4)
p.int <- zapsmall( aov.sum[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.int <- zapsmall( aov.sum[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X int 
f.n2i <- zapsmall( aov.sum[[6]][[1]][["F value"]][1], digits = 4)
p.n2i <- zapsmall( aov.sum[[6]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2i <- zapsmall( aov.sum[[6]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum[[5]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum[[5]][[1]][["Mean Sq"]][2], digits = 4)
#n2num X rel X int
f.2ri <- zapsmall( aov.sum[[8]][[1]][["F value"]][1], digits = 4)
p.2ri <- zapsmall( aov.sum[[8]][[1]][["Pr(>F)"]][1],  digits = 4)
m.2ri <- zapsmall( aov.sum[[8]][[1]][["Mean Sq"]][2], digits = 4)


error.rate.ANOVA.1 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness",
    "Integration",
    "Local Noun Number X Integration",
    "Relatedness X Integration",
    "Local Noun Number X Relatedness X Integration"
  ),
  
  F1    = c(
    f.n2n,
    f.rel,
    f.n2r,
    f.int,
    f.n2i,
    f.rXi,
    f.2ri
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n),")", sep = ""),
    paste( m.rel," (", get_stars( p.rel),")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r),")", sep = ""),
    paste( m.int," (", get_stars( p.int),")", sep = ""),
    paste( m.n2i," (", get_stars( p.n2i),")", sep = ""),
    paste( m.rXi," (", get_stars( p.rXi),")", sep = ""),
    paste( m.2ri," (", get_stars( p.2ri),")", sep = ""))   
)


# F2


source( file = "semrel_uninflected_f2_read_in_dataframe.R")


a.2x2x2.f2 <- aov(unind ~ semint * related * n2num + Error( item / ( semint * related * n2num)), data = d.sr)

sink("output/table17_SR_uninflected_f2_anova.txt")
cat("Table 17: F2 Uninflected 2 x 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
print( summary( a.2x2x2.f2), digits = 6)
sink()

aov.sum2 <- summary( a.2x2x2.f2)

#n2num
f.n2n <- zapsmall( aov.sum2[[4]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum2[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum2[[4]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum2[[3]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum2[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum2[[3]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum2[[7]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum2[[7]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum2[[7]][[1]][["Mean Sq"]][2], digits = 4)
#semint
f.int <- zapsmall( aov.sum2[[2]][[1]][["F value"]][1], digits = 4)
p.int <- zapsmall( aov.sum2[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.int <- zapsmall( aov.sum2[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X int 
f.n2i <- zapsmall( aov.sum2[[6]][[1]][["F value"]][1], digits = 4)
p.n2i <- zapsmall( aov.sum2[[6]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2i <- zapsmall( aov.sum2[[6]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum2[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum2[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum2[[5]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum2[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum2[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum2[[5]][[1]][["Mean Sq"]][2], digits = 4)
#n2num X rel X int
f.2ri <- zapsmall( aov.sum2[[8]][[1]][["F value"]][1], digits = 4)
p.2ri <- zapsmall( aov.sum2[[8]][[1]][["Pr(>F)"]][1],  digits = 4)
m.2ri <- zapsmall( aov.sum2[[8]][[1]][["Mean Sq"]][2], digits = 4)

error.rate.ANOVA.2 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness",
    "Integration",
    "Local Noun Number X Integration",
    "Relatedness X Integration",
    "Local Noun Number X Relatedness X Integration"
  ),
  
  F2    = c(
    f.n2n,
    f.rel,
    f.n2r,
    f.int,
    f.n2i,
    f.rXi,
    f.2ri
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n), ")", sep = ""),
    paste( m.rel," (", get_stars( p.rel), ")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r), ")", sep = ""),
    paste( m.int," (", get_stars( p.int), ")", sep = ""),
    paste( m.n2i," (", get_stars( p.n2i), ")", sep = ""),
    paste( m.rXi," (", get_stars( p.rXi), ")", sep = ""),
    paste( m.2ri," (", get_stars( p.2ri), ")", sep = ""))   
)


error.rate.ANOVA <- cbind(error.rate.ANOVA.1,error.rate.ANOVA.2[,c(2:3)])


write.xlsx(error.rate.ANOVA, file="output/table17_SR_uninflected_anovas.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)

#-------------MISCELLANEOUS
# 

source( file = "clear_and_setup.R")
source( file = "semrel_error_data_read_in_dataframe.R")

a.2x2x2.f1 <- aov(misc ~ integrated * related * n2num + Error( subject / ( integrated * related * n2num)), data = d.sr)

 sink("output/table18_SR_miscellaneous_f1_anova.txt")
cat("Table 18: F1 Miscellaneous 2 x 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)
# print( summary( a.2x2x2.f1), digits = 6)
# sink()
aov.sum <- summary( a.2x2x2.f1)

#n2num
f.n2n <- zapsmall( aov.sum[[4]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum[[4]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum[[3]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum[[3]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum[[7]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum[[7]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum[[7]][[1]][["Mean Sq"]][2], digits = 4)
#semint
f.int <- zapsmall( aov.sum[[2]][[1]][["F value"]][1], digits = 4)
p.int <- zapsmall( aov.sum[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.int <- zapsmall( aov.sum[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X int 
f.n2i <- zapsmall( aov.sum[[6]][[1]][["F value"]][1], digits = 4)
p.n2i <- zapsmall( aov.sum[[6]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2i <- zapsmall( aov.sum[[6]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum[[5]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum[[5]][[1]][["Mean Sq"]][2], digits = 4)
#n2num X rel X int
f.2ri <- zapsmall( aov.sum[[8]][[1]][["F value"]][1], digits = 4)
p.2ri <- zapsmall( aov.sum[[8]][[1]][["Pr(>F)"]][1],  digits = 4)
m.2ri <- zapsmall( aov.sum[[8]][[1]][["Mean Sq"]][2], digits = 4)


error.rate.ANOVA.1 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness",
    "Integration",
    "Local Noun Number X Integration",
    "Relatedness X Integration",
    "Local Noun Number X Relatedness X Integration"
  ),
  
  F1    = c(
    f.n2n,
    f.rel,
    f.n2r,
    f.int,
    f.n2i,
    f.rXi,
    f.2ri
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n),")", sep = ""),
    paste( m.rel," (", get_stars( p.rel),")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r),")", sep = ""),
    paste( m.int," (", get_stars( p.int),")", sep = ""),
    paste( m.n2i," (", get_stars( p.n2i),")", sep = ""),
    paste( m.rXi," (", get_stars( p.rXi),")", sep = ""),
    paste( m.2ri," (", get_stars( p.2ri),")", sep = ""))   
)


# F2
source( file = "semrel_uninflected_f2_read_in_dataframe.R")
a.2x2x2.f2 <- aov(misc ~ semint * related * n2num + Error( item / ( semint * related * n2num)), data = d.sr)

sink("output/table18_SR_miscellaneous_f2_anova.txt")
cat("Table 18: F2 Miscellaneous 2 x 2 x 2 ANOVA", format( Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 80)

print( summary( a.2x2x2.f2), digits = 6)
sink()

aov.sum2 <- summary( a.2x2x2.f2)

#n2num
f.n2n <- zapsmall( aov.sum2[[4]][[1]][["F value"]][1], digits = 4)
p.n2n <- zapsmall( aov.sum2[[4]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2n <- zapsmall( aov.sum2[[4]][[1]][["Mean Sq"]][2], digits = 4)
#related
f.rel <- zapsmall( aov.sum2[[3]][[1]][["F value"]][1], digits = 4)
p.rel <- zapsmall( aov.sum2[[3]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rel <- zapsmall( aov.sum2[[3]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X rel
f.n2r <- zapsmall( aov.sum2[[7]][[1]][["F value"]][1], digits = 4)
p.n2r <- zapsmall( aov.sum2[[7]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2r <- zapsmall( aov.sum2[[7]][[1]][["Mean Sq"]][2], digits = 4)
#semint
f.int <- zapsmall( aov.sum2[[2]][[1]][["F value"]][1], digits = 4)
p.int <- zapsmall( aov.sum2[[2]][[1]][["Pr(>F)"]][1],  digits = 4)
m.int <- zapsmall( aov.sum2[[2]][[1]][["Mean Sq"]][2], digits = 4)
#n2n X int 
f.n2i <- zapsmall( aov.sum2[[6]][[1]][["F value"]][1], digits = 4)
p.n2i <- zapsmall( aov.sum2[[6]][[1]][["Pr(>F)"]][1],  digits = 4)
m.n2i <- zapsmall( aov.sum2[[6]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum2[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum2[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum2[[5]][[1]][["Mean Sq"]][2], digits = 4)
#rel X int
f.rXi <- zapsmall( aov.sum2[[5]][[1]][["F value"]][1], digits = 4)
p.rXi <- zapsmall( aov.sum2[[5]][[1]][["Pr(>F)"]][1],  digits = 4)
m.rXi <- zapsmall( aov.sum2[[5]][[1]][["Mean Sq"]][2], digits = 4)
#n2num X rel X int
f.2ri <- zapsmall( aov.sum2[[8]][[1]][["F value"]][1], digits = 4)
p.2ri <- zapsmall( aov.sum2[[8]][[1]][["Pr(>F)"]][1],  digits = 4)
m.2ri <- zapsmall( aov.sum2[[8]][[1]][["Mean Sq"]][2], digits = 4)

error.rate.ANOVA.2 <- data.frame(
  
  Effect = c(
    "Local Noun Number",
    "Related",
    "Local Noun Number X Relatedness",
    "Integration",
    "Local Noun Number X Integration",
    "Relatedness X Integration",
    "Local Noun Number X Relatedness X Integration"
  ),
  
  F2    = c(
    f.n2n,
    f.rel,
    f.n2r,
    f.int,
    f.n2i,
    f.rXi,
    f.2ri
  ),
  
  MSe   =c(
    paste( m.n2n," (", get_stars( p.n2n), ")", sep = ""),
    paste( m.rel," (", get_stars( p.rel), ")", sep = ""),
    paste( m.n2r," (", get_stars( p.n2r), ")", sep = ""),
    paste( m.int," (", get_stars( p.int), ")", sep = ""),
    paste( m.n2i," (", get_stars( p.n2i), ")", sep = ""),
    paste( m.rXi," (", get_stars( p.rXi), ")", sep = ""),
    paste( m.2ri," (", get_stars( p.2ri), ")", sep = ""))   
)


error.rate.ANOVA <- cbind(error.rate.ANOVA.1,error.rate.ANOVA.2[,c(2:3)])


write.xlsx(error.rate.ANOVA, file="output/table18_SR_miscellaneous_anovas.xlsx", col.names = TRUE, row.names = FALSE, append = FALSE)

