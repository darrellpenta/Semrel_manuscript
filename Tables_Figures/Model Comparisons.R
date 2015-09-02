rm( list=ls( ))
# ----------------- >> Load Libraries --------------------------------------------------

library( package = languageR)
library( package = lmerTest)
library( lme4)
source( "Model Comparisons_dataframe_setup.R")

# ---------------------0.0 DOCUMENT SETUP ----------------------------------------------------------------------------------------
line = rep( c( "-"), times = 40, fill = 80)
br   = "\n"
sink( "output/Model Comparisons Results Summary.txt")
cat( format( Sys.time( ), "%b. %d, %Y at %T"), sep = "", fill = 80)
# -------------------- 1.0 SEMREL: ANALYSES  ------------------------------------------
# -------------------- 1.1 SEMREL: SET UP SECTION  ---------------------------------
cat( rep( c( "*"), times = 50, quote = F), br)
cat( format( " SEMREL"), sep = "", fill = 80)
cat( rep( c( "*"), times = 50, quote = F), br, br)



# -------------------- 1.2 SEMREL: CREATE AND COMPARE MODELS --------------------------
# ---------------------- 1. REL * INT ---------------------------------------------------------------
cat( br, line, br, "1. Rel * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m1.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m1.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m1.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel+int| item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m1.f2), corr = FALSE)


# ---------------------- 1a. REL * INT ( RES) ---------------------------------------------------------------
cat( br, br, line, br, "1a. Rel * Int (RES)", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
cat( "    **NOTE: Model converges with either factor alone in the raandom effects, as well as with both factors as main effects--- the problem is with including the interaction", br, br)
m1a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus +   rel * res.int + ( 1 + rel + res.int |subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m1a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m1a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 +  rel + res.int |item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m1a.f2), corr = FALSE)

# ---------------------- 1b. REL (RES) * INT ---------------------------------------------------------------
cat( br, br, line, br, "1b. Rel(RES) * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m1b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m1b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m1b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 +  res.rel+int |item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m1b.f2), corr = FALSE)

# ---------------------- 2.  INT * ASSHL ---------------------------------------------------------------
cat( br, br, line, br,"2. Int * AssHL", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m2.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m2.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m2.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int + assHL | item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m2.f2), corr = FALSE)


# ---------------------- 3. REL * ASSHL  ---------------------------------------------------------------
cat( br, br, line, br,"3. Rel * AssHL", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m3.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m3.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m3.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel + assHL | item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m3.f2), corr = FALSE)

# ---------------------- 3a. REL * ASSHL (RES)  ---------------------------------------------------------------

cat( br, br, line, br,"3a. Rel * AssHL (RES)", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m3a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m3a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m3a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel + res.asshl | item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m3a.f2), corr = FALSE)

# ---------------------- 3b. REL (RES) * ASSHL   ---------------------------------------------------------------

cat( br, br, line, br,"3b. Rel (Res) * AssHL ", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m3b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
print( summary( m3b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m3b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2 + assHL | item), data = f2.sr, weights = ( 1/v), REML = FALSE)
print( summary( m3b.f2), corr = FALSE)

# -------------------------------------------------------------------------------------------------------------------------
# -------------------- 10.1 CAT: SET UP SECTION  ---------------------------------
cat( br,br,rep( c( "*"), times = 50, quote = F), br)
cat( format( " CATEGORY COORDINATES"), sep = "", fill = 80)
cat( rep( c( "*"), times = 50, quote = F), br, br)

# ------------------ 10.2 CAT: CREATE AND COMPARE MODELS------------------------------
# ---------------------- 10. REL * INT ---------------------------------------------------------------
cat( br, line, br, "10. Rel * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m10.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m10.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)
m10.f22 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1| item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m10.f22), corr = FALSE)


# ---------------------- 10a. REL * INT ( RES) ---------------------------------------------------------------
cat( br, br, line, br, "10a. Rel * Int (RES)", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m10a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m10a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)
m10a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 + rel:res.int|item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m10a.f2), corr = FALSE)

# ---------------------- 10b. REL (RES) * INT ---------------------------------------------------------------
cat( br, br, line, br, "10b. Rel(RES) * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m10b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m10b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)
m10b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1  |item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m10b.f2), corr = FALSE)

# ---------------------- 20.  INT * ASSHL ---------------------------------------------------------------
cat( br, br, line, br,"20. Int * AssHL", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
cat( "    **NOTE: Model wont converge with Association in the random effects structure", br, br)
m20.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 + int | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m20.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)
m20.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1| item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m20.f2), corr = FALSE)


# ---------------------- 30. REL * ASSHL  ---------------------------------------------------------------
cat( br, br, line, br,"30. Rel * AssHL", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)
m30.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1  | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m30.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")

cat( "    **NOTE: Model only converges with random intercept", br, br)
m30.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1  | item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m30.f2), corr = FALSE)

# ---------------------- 30a. REL * ASSHL (RES)  ---------------------------------------------------------------

cat( br, br, line, br,"30a. Rel * AssHL (RES)", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m30a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m30a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)

m30a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 | item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m30a.f2), corr = FALSE)

# ---------------------- 30b. REL (RES) * ASSHL   ---------------------------------------------------------------

cat( br, br, line, br,"30b. Rel (Res) * AssHL ", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m30b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 + assHL | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)
print( summary( m30b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)


m30b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 | item), data = f2.cc, weights = ( 1/v), REML = FALSE)
print( summary( m30b.f2), corr = FALSE)

# --------------------------------------------------------------------------------------------------------------------------
# ------------------ 100.0 PROP: ANALYSES ----------------------------------------------
# ------------------ 100.1 PROP: SET UP SECTION  ---------------------------------
cat( br,br,rep( c( "*"), times = 50, quote = F), br)
cat( format( " PROPERTY ITEMS"), sep = "", fill = 80)
cat( rep( c( "*"), times = 50, quote = F), br, br)

# ------------------ 100.2 PROP: CREATE AND COMPARE MODELS------------------------------

# ---------------------- 100. REL * INT ---------------------------------------------------------------
cat( br, line, br, "100. Rel * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m100.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m100.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m100.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel:int| item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m100.f2), corr = FALSE)


# ---------------------- 100a. REL * INT ( RES) ---------------------------------------------------------------
cat( br, br, line, br, "100a. Rel * Int (RES)", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m100a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m100a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m100a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 +  rel:res.int |item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m100a.f2), corr = FALSE)

# ---------------------- 100b. REL (RES) * INT ---------------------------------------------------------------
cat( br, br, line, br, "100b. Rel(RES) * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m100b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m100b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model only converges with random intercept", br, br)

m100b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 |item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m100b.f2), corr = FALSE)

# ---------------------- 200.  INT * ASSHL ---------------------------------------------------------------
cat( br, br, line, br,"200. Int * AssHL", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m200.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m200.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m200.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int: assHL | item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m200.f2), corr = FALSE)


# ---------------------- 300. REL * ASSHL  ---------------------------------------------------------------
cat( br, br, line, br,"300. Rel * AssHL", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m300.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m300.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m300.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel:assHL | item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m300.f2), corr = FALSE)

# ---------------------- 300a. REL * ASSHL (RES)  ---------------------------------------------------------------

cat( br, br, line, br,"300a. Rel * AssHL (RES)", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m300a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m300a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m300a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel: res.asshl | item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m300a.f2), corr = FALSE)

# ---------------------- 300b. REL (RES) * ASSHL   ---------------------------------------------------------------

cat( br, br, line, br,"300b. Rel (Res) * AssHL ", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m300b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)
print( summary( m300b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m300b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2 :assHL | item), data = f2.pr, weights = ( 1/v), REML = FALSE)
print( summary( m300b.f2), corr = FALSE)

# -------------------------------------------------------------------------------------------------------------------------
## ------------------- 1000.0 All: ANALYSES -----------------------------------------------
## ------------------- 1000.1 All: SET UP OUTPUT FILE -------------------------------------
# -------------------- 1000.1 ALL: SET UP SECTION  ---------------------------------
cat( rep( c( "*"), times = 50, quote = F), br)
cat( format( " ALL"), sep = "", fill = 80)
cat( rep( c( "*"), times = 50, quote = F), br, br)
# ---------------------- 1000. REL * INT ---------------------------------------------------------------
cat( br, line, br, "1000. Rel * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m1000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m1000.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m1000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel:int| item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m1000.f2), corr = FALSE)


# ---------------------- 1000a. REL * INT ( RES) ---------------------------------------------------------------
cat( br, br, line, br, "1000a. Rel * Int (RES)", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m1000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m1000a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m1000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1  + rel:res.int|item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m1000a.f2), corr = FALSE)

# ---------------------- 1000b. REL (RES) * INT ---------------------------------------------------------------
cat( br, br, line, br, "1000b. Rel(RES) * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m1000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m1000b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m1000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 +  res.rel:int |item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m1000b.f2), corr = FALSE)

# ---------------------- 2000.  INT * ASSHL ---------------------------------------------------------------
cat( br, br, line, br,"2000. Int * AssHL", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m2000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m2000.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m2000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int: assHL | item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m2000.f2), corr = FALSE)


# ---------------------- 3000. REL * ASSHL  ---------------------------------------------------------------
cat( br, br, line, br,"3000. Rel * AssHL", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m3000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m3000.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m3000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel:assHL | item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m3000.f2), corr = FALSE)

# ---------------------- 3000a. REL * ASSHL (RES)  ---------------------------------------------------------------

cat( br, br, line, br,"3000a. Rel * AssHL (RES)", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m3000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m3000a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction or main effects in Random Effects, so only using higher order interaction", br, br)
m3000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel:res.asshl | item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m3000a.f2), corr = FALSE)

# ---------------------- 3000b. REL (RES) * ASSHL   ---------------------------------------------------------------

cat( br, br, line, br,"3000b. Rel (Res) * AssHL ", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m3000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.all, weights = ( 1/v), REML = FALSE)
print( summary( m3000b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m3000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2: assHL | item), data = f2.all, weights = ( 1/v), REML = FALSE)
print( summary( m3000b.f2), corr = FALSE)


# -------------------------------------------------------------------------------------------------------------------------
## ------------------- 10000.0 SR & PROP: ANALYSES -----------------------------------------------

# -------------------- 10000.1 SR & PROP: SET UP SECTION  ---------------------------------
cat( rep( c( "*"), times = 50, quote = F), br)
cat( format( " SR & PROP"), sep = "", fill = 80)
cat( rep( c( "*"), times = 50, quote = F), br, br)
# ---------------------- 10000.2 SR & PROP: CREATE AND COMPARE MODELS --------------------------
# ---------------------- 10000. REL * INT ---------------------------------------------------------------
cat( br, line, br, "10000. Rel * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m10000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m10000.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m10000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel+int| item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m10000.f2), corr = FALSE)


# ---------------------- 10000a. REL * INT ( RES) ---------------------------------------------------------------
cat( br, br, line, br, "10000a. Rel * Int (RES)", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m10000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m10000a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m10000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 +  rel: res.int |item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m10000a.f2), corr = FALSE)

# ---------------------- 10000b. REL (RES) * INT ---------------------------------------------------------------
cat( br, br, line, br, "10000b. Rel(RES) * Int", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m10000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m10000b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m10000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 +  res.rel+int |item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m10000b.f2), corr = FALSE)

# ---------------------- 20000.  INT * ASSHL ---------------------------------------------------------------
cat( br, br, line, br,"20000. Int * AssHL", br, line, br, sep = " ")

#F1
cat( br, br, "F1", br, line, br, sep = " ")
m20000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m20000.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m20000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int + assHL | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m20000.f2), corr = FALSE)


# ---------------------- 30000. REL * ASSHL  ---------------------------------------------------------------
cat( br, br, line, br,"30000. Rel * AssHL", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m30000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m30000.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m30000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel + assHL | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m30000.f2), corr = FALSE)

# ---------------------- 30000a. REL * ASSHL (RES)  ---------------------------------------------------------------

cat( br, br, line, br,"30000a. Rel * AssHL (RES)", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m30000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m30000a.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m30000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel + res.asshl | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m30000a.f2), corr = FALSE)

# ---------------------- 30000b. REL (RES) * ASSHL   ---------------------------------------------------------------

cat( br, br, line, br,"30000b. Rel (Res) * AssHL ", br, line, br, sep = " ")


#F1
cat( br, br, "F1", br, line, br, sep = " ")
m30000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary( m30000b.f1), corr = FALSE)

#F2
cat( br, br, "F2", br, line, br, sep = " ")
cat( "    **NOTE: Model doesn't converge with interaction in Random Effects, so only using main effects", br, br)
m30000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2 + assHL | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)
print( summary (m30000b.f2), corr = FALSE)
sink( )
