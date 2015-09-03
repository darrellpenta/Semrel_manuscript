rm( list=ls( ))
# ----------------- >> Load Libraries --------------------------------------------------

library( package = languageR)
library( package = lmerTest)
library( lme4)
source( "Model Comparisons_dataframe_setup.R")

# ---------------------- 1. REL * INT ---------------------------------------------------------------
#F1
m1.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)

#F2
m1.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel+int| item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# ---------------------- 1a. REL * INT ( RES) ---------------------------------------------------------------
#F1
m1a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus +   rel * res.int + ( 1 + rel + res.int |subject), data = f1.sr, weights = ( 1/v), REML = FALSE)

#F2
m1a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 +  rel + res.int |item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# ---------------------- 1b. REL (RES) * INT ---------------------------------------------------------------
#F1
m1b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.sr, weights = ( 1/v), REML = FALSE)
#F2
m1b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 +  res.rel+int |item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# ---------------------- 2.  INT * ASSHL ---------------------------------------------------------------
#F1
m2.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)

#F2
m2.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int + assHL | item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# ---------------------- 3. REL * ASSHL  ---------------------------------------------------------------

#F1
m3.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)

#F2
m3.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel + assHL | item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# ---------------------- 3a. REL * ASSHL (RES)  ---------------------------------------------------------------
#F1
m3a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)

#F2
m3a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel + res.asshl | item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# ---------------------- 3b. REL (RES) * ASSHL   ---------------------------------------------------------------
#F1
m3b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.sr, weights = ( 1/v), REML = FALSE)

#F2
m3b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2 + assHL | item), data = f2.sr, weights = ( 1/v), REML = FALSE)

# -------------------------------------------------------------------------------------------------------------------------
# -------------------- 10.1 CAT: SET UP SECTION  ---------------------------------
# ------------------ 10.2 CAT: CREATE AND COMPARE MODELS------------------------------
# ---------------------- 10. REL * INT ---------------------------------------------------------------

#F1
m10.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m10.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1| item), data = f2.cc, weights = ( 1/v), REML = FALSE)

# ---------------------- 10a. REL * INT ( RES) ---------------------------------------------------------------

#F1
m10a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m10a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 |item), data = f2.cc, weights = ( 1/v), REML = FALSE)

# ---------------------- 10b. REL (RES) * INT ---------------------------------------------------------------
#F1
m10b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m10b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1  |item), data = f2.cc, weights = ( 1/v), REML = FALSE)

# ---------------------- 20.  INT * ASSHL ---------------------------------------------------------------
#F1
m20.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 + int | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m20.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1| item), data = f2.cc, weights = ( 1/v), REML = FALSE)


# ---------------------- 30. REL * ASSHL  ---------------------------------------------------------------
#F1
m30.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1  | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m30.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1  | item), data = f2.cc, weights = ( 1/v), REML = FALSE)

# ---------------------- 30a. REL * ASSHL (RES)  ---------------------------------------------------------------
#F1
m30a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m30a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 | item), data = f2.cc, weights = ( 1/v), REML = FALSE)

# ---------------------- 30b. REL (RES) * ASSHL   ---------------------------------------------------------------
#F1
m30b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 + assHL | subject), data = f1.cc, weights = ( 1/v), REML = FALSE)

#F2
m30b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 | item), data = f2.cc, weights = ( 1/v), REML = FALSE)

# --------------------------------------------------------------------------------------------------------------------------
# ------------------ 100.0 PROP: ANALYSES ----------------------------------------------
# ------------------ 100.2 PROP: CREATE AND COMPARE MODELS------------------------------

# ---------------------- 100. REL * INT ---------------------------------------------------------------

#F1
m100.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m100.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel:int| item), data = f2.pr, weights = ( 1/v), REML = FALSE)


# ---------------------- 100a. REL * INT ( RES) ---------------------------------------------------------------
#F1
m100a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m100a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 +  rel:res.int |item), data = f2.pr, weights = ( 1/v), REML = FALSE)

# ---------------------- 100b. REL (RES) * INT ---------------------------------------------------------------
#F1
m100b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m100b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 |item), data = f2.pr, weights = ( 1/v), REML = FALSE)

# ---------------------- 200.  INT * ASSHL ---------------------------------------------------------------
#F1
m200.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m200.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int: assHL | item), data = f2.pr, weights = ( 1/v), REML = FALSE)


# ---------------------- 300. REL * ASSHL  ---------------------------------------------------------------

#F1
m300.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m300.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel:assHL | item), data = f2.pr, weights = ( 1/v), REML = FALSE)

# ---------------------- 300a. REL * ASSHL (RES)  ---------------------------------------------------------------
#F1
m300a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m300a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel: res.asshl | item), data = f2.pr, weights = ( 1/v), REML = FALSE)

# ---------------------- 300b. REL (RES) * ASSHL   ---------------------------------------------------------------
#F1
m300b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.pr, weights = ( 1/v), REML = FALSE)

#F2
m300b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2 :assHL | item), data = f2.pr, weights = ( 1/v), REML = FALSE)

# -------------------------------------------------------------------------------------------------------------------------
## ------------------- 1000.0 All: ANALYSES -----------------------------------------------
# ---------------------- 1000. REL * INT ---------------------------------------------------------------
#F1
m1000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m1000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel:int| item), data = f2.all, weights = ( 1/v), REML = FALSE)

# ---------------------- 1000a. REL * INT ( RES) ---------------------------------------------------------------

#F1
m1000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m1000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1  + rel:res.int|item), data = f2.all, weights = ( 1/v), REML = FALSE)

# ---------------------- 1000b. REL (RES) * INT ---------------------------------------------------------------

#F1
m1000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m1000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 +  res.rel:int |item), data = f2.all, weights = ( 1/v), REML = FALSE)

# ---------------------- 2000.  INT * ASSHL ---------------------------------------------------------------
#F1
m2000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m2000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int: assHL | item), data = f2.all, weights = ( 1/v), REML = FALSE)


# ---------------------- 3000. REL * ASSHL  ---------------------------------------------------------------
#F1
m3000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m3000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel:assHL | item), data = f2.all, weights = ( 1/v), REML = FALSE)

# ---------------------- 3000a. REL * ASSHL (RES)  ---------------------------------------------------------------
#F1
m3000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m3000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel:res.asshl | item), data = f2.all, weights = ( 1/v), REML = FALSE)

# ---------------------- 3000b. REL (RES) * ASSHL   --------------------------------------------------------------
#F1
m3000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.all, weights = ( 1/v), REML = FALSE)

#F2
m3000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2: assHL | item), data = f2.all, weights = ( 1/v), REML = FALSE)

# -------------------------------------------------------------------------------------------------------------------------
## ------------------- 10000.0 SR & PROP: ANALYSES -----------------------------------------------
# ---------------------- 10000. REL * INT ---------------------------------------------------------------
#F1
m10000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int + ( 1 +  rel * int | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m10000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * int  + ( 1 +  rel+int| item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)

# ---------------------- 10000a. REL * INT ( RES) ---------------------------------------------------------------

#F1
m10000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int + ( 1 +  rel *res.int |subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m10000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.int  + ( 1 +  rel: res.int |item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)

# ---------------------- 10000b. REL (RES) * INT ---------------------------------------------------------------

#F1
m10000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int + ( 1 +  res.rel * int |subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m10000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel * int  + ( 1 +  res.rel+int |item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)

# ---------------------- 20000.  INT * ASSHL ---------------------------------------------------------------
#F1
m20000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL + ( 1 +  int * assHL | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m20000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + int * assHL  + ( 1 +  int + assHL | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)


# ---------------------- 30000. REL * ASSHL  ---------------------------------------------------------------
#F1
m30000.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL + ( 1 +  rel * assHL | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m30000.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * assHL  + ( 1 +  rel + assHL | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)

# ---------------------- 30000a. REL * ASSHL (RES)  --------------------------------------------------------------#F1
m30000a.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl + ( 1 +  rel * res.asshl | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m30000a.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + rel * res.asshl  + ( 1 +  rel + res.asshl | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)

# ---------------------- 30000b. REL (RES) * ASSHL   -------------------------------------------------------------#F1
m30000b.f1 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL + ( 1 +  res.rel2 * assHL | subject), data = f1.sr.prop, weights = ( 1/v), REML = FALSE)

#F2
m30000b.f2 <- lmer( elog ~  1 + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + plaus + res.rel2 * assHL  + ( 1 +  res.rel2 + assHL | item), data = f2.sr.prop, weights = ( 1/v), REML = FALSE)


s.m1.f1      <- summary( m1.f1)
s.m1.f2      <- summary( m1.f2)
s.m2.f1      <- summary( m2.f1)
s.m2.f2      <- summary( m2.f2)
s.m3.f1      <- summary( m3.f1)
s.m3.f2      <- summary( m3.f2)
s.m10.f1     <- summary( m10.f1)
s.m10.f2     <- summary( m10.f2)
s.m30.f1     <- summary( m30.f1)
s.m30.f2     <- summary( m30.f2)
s.m100.f1    <- summary( m100.f1)
s.m100.f2    <- summary( m100.f2)
s.m200.f1    <- summary( m200.f1)
s.m200.f2    <- summary( m200.f2)
s.m300.f1    <- summary( m300.f1)
s.m300.f2    <- summary( m300.f2)
s.m10000.f1  <- summary( m10000.f1)
s.m10000.f2  <- summary( m10000.f2)
s.m20000.f1  <- summary( m20000.f1)    
s.m20000.f2  <- summary( m20000.f2)
s.m30000.f1  <- summary( m30000.f1)
s.m30000.f2  <- summary( m30000.f2)
s.m1000.f1   <- summary( m1000.f1)
s.m1000.f2   <- summary( m1000.f2)
s.m2000.f1   <- summary( m2000.f1)    
s.m2000.f2   <- summary( m2000.f2)
s.m3000.f1   <- summary( m3000.f1)
s.m3000.f2   <- summary( m3000.f2)
s.m1a.f1      <- summary( m1a.f1)
s.m1a.f2      <- summary( m1a.f2)
s.m3a.f1      <- summary( m3a.f1)
s.m3a.f2      <- summary( m3a.f2)
s.m10a.f1     <- summary( m10a.f1)
s.m10a.f2     <- summary( m10a.f2)
s.m30a.f1     <- summary( m30a.f1)
s.m30a.f2     <- summary( m30a.f2)
s.m100a.f1    <- summary( m100a.f1)
s.m100a.f2    <- summary( m100a.f2)
s.m300a.f1    <- summary( m300a.f1)
s.m300a.f2    <- summary( m300a.f2)
s.m10000a.f1  <- summary( m10000a.f1)
s.m10000a.f2  <- summary( m10000a.f2)
s.m30000a.f1  <- summary( m30000a.f1)
s.m30000a.f2  <- summary( m30000a.f2)
s.m1000a.f1   <- summary( m1000a.f1)
s.m1000a.f2   <- summary( m1000a.f2)
s.m3000a.f1   <- summary( m3000a.f1)
s.m3000a.f2   <- summary( m3000a.f2)
s.m1b.f1      <- summary( m1b.f1)
s.m1b.f2      <- summary( m1b.f2)
s.m3b.f1      <- summary( m3b.f1)
s.m3b.f2      <- summary( m3b.f2)
s.m10b.f1     <- summary( m10b.f1)
s.m10b.f2     <- summary( m10b.f2)
s.m30b.f1     <- summary( m30b.f1)
s.m30b.f2     <- summary( m30b.f2)
s.m100b.f1    <- summary( m100b.f1)
s.m100b.f2    <- summary( m100b.f2)
s.m300b.f1    <- summary( m300b.f1)
s.m300b.f2    <- summary( m300b.f2)
s.m10000b.f1  <- summary( m10000b.f1)
s.m10000b.f2  <- summary( m10000b.f2)
s.m30000b.f1  <- summary( m30000b.f1)
s.m30000b.f2  <- summary( m30000b.f2)
s.m1000b.f1   <- summary( m1000b.f1)
s.m1000b.f2   <- summary( m1000b.f2)
s.m3000b.f1   <- summary( m3000b.f1)
s.m3000b.f2   <- summary( m3000b.f2)

p.s.m1.f1.r      <-  zapsmall( s.m1.f1[[10]][[53]], digits = 3)      
p.s.m10.f1.r     <-  zapsmall( s.m10.f1[[10]][[53]], digits = 3)     
p.s.m100.f1.r    <-  zapsmall( s.m100.f1[[10]][[53]], digits = 3)    
p.s.m10000.f1.r  <-  zapsmall( s.m10000.f1[[10]][[53]], digits = 3)  
p.s.m1000.f1.r   <-  zapsmall( s.m1000.f1[[10]][[53]], digits = 3)   



p.s.m1a.f1.r     <-  zapsmall( s.m1a.f1[[10]][[53]], digits = 3)     
p.s.m10a.f1.r    <-  zapsmall( s.m10a.f1[[10]][[53]], digits = 3)    
p.s.m100a.f1.r   <-  zapsmall( s.m100a.f1[[10]][[53]], digits = 3)   
p.s.m10000a.f1.r <-  zapsmall( s.m10000a.f1[[10]][[53]], digits = 3) 
p.s.m1000a.f1.r  <-  zapsmall( s.m1000a.f1[[10]][[53]], digits = 3)  



p.s.m1b.f1.r     <-  zapsmall( s.m1b.f1[[10]][[53]], digits = 3)     
p.s.m10b.f1.r    <-  zapsmall( s.m10b.f1[[10]][[53]], digits = 3)    
p.s.m100b.f1.r   <-  zapsmall( s.m100b.f1[[10]][[53]], digits = 3)   
p.s.m10000b.f1.r <-  zapsmall( s.m10000b.f1[[10]][[53]], digits = 3) 
p.s.m1000b.f1.r  <-  zapsmall( s.m1000b.f1[[10]][[53]], digits = 3)  



p.s.m2.f1.r      <-  zapsmall( s.m2.f1[[10]][[54]], digits = 3)      
# p.s.m20.f1.r     <-  zapsmall( s.m20.f1[[10]][[53]], digits = 3)     
p.s.m200.f1.r    <-  zapsmall( s.m200.f1[[10]][[54]], digits = 3)    
p.s.m20000.f1.r  <-  zapsmall( s.m20000.f1[[10]][[54]], digits = 3)  
p.s.m2000.f1.r   <-  zapsmall( s.m2000.f1[[10]][[54]], digits = 3)   



p.s.m3.f1.r      <-  zapsmall( s.m3.f1[[10]][[53]], digits = 3)      
# p.s.m30.f1.r     <-  zapsmall( s.m30.f1[[10]][[53]], digits = 3)     
p.s.m300.f1.r    <-  zapsmall( s.m300.f1[[10]][[53]], digits = 3)    
p.s.m30000.f1.r  <-  zapsmall( s.m30000.f1[[10]][[53]], digits = 3)  
p.s.m3000.f1.r   <-  zapsmall( s.m3000.f1[[10]][[53]], digits = 3)   



p.s.m3a.f1.r     <-  zapsmall( s.m3a.f1[[10]][[53]], digits = 3)     
# p.s.m30a.f1.r    <-  zapsmall( s.m30a.f1[[10]][[53]], digits = 3)    
p.s.m300a.f1.r   <-  zapsmall( s.m300a.f1[[10]][[53]], digits = 3)   
p.s.m30000a.f1.r <-  zapsmall( s.m30000a.f1[[10]][[53]], digits = 3) 
p.s.m3000a.f1.r  <-  zapsmall( s.m3000a.f1[[10]][[53]], digits = 3)  



p.s.m3b.f1.r     <-  zapsmall( s.m3b.f1[[10]][[53]], digits = 3)     
# p.s.m30b.f1.r    <-  zapsmall( s.m30b.f1[[10]][[53]], digits = 3)    
p.s.m300b.f1.r   <-  zapsmall( s.m300b.f1[[10]][[53]], digits = 3)   
p.s.m30000b.f1.r <-  zapsmall( s.m30000b.f1[[10]][[53]], digits = 3) 
p.s.m3000b.f1.r  <-  zapsmall( s.m3000b.f1[[10]][[53]], digits = 3)  



p.s.m1.f2.r <-  zapsmall( s.m1.f2[[10]][[53]], digits = 3)      
p.s.m10.f2.r <-  zapsmall( s.m10.f2[[10]][[53]], digits = 3)     
p.s.m100.f2.r <-  zapsmall( s.m100.f2[[10]][[53]], digits = 3)    
p.s.m10000.f2.r  <-  zapsmall( s.m10000.f2[[10]][[53]], digits = 3)  
p.s.m1000.f2.r <-  zapsmall( s.m1000.f2[[10]][[53]], digits = 3)   



p.s.m1a.f2.r      <-  zapsmall( s.m1a.f2[[10]][[53]], digits = 3)     
# p.s.m10a.f2.r   <-  zapsmall( s.m10a.f2[[10]][[53]], digits = 3)    
p.s.m100a.f2.r    <-  zapsmall( s.m100a.f2[[10]][[53]], digits = 3)   
p.s.m10000a.f2.r  <-  zapsmall( s.m10000a.f2[[10]][[53]], digits = 3) 
p.s.m1000a.f2.r   <-  zapsmall( s.m1000a.f2[[10]][[53]], digits = 3)  



p.s.m1b.f2.r      <-  zapsmall( s.m1b.f2[[10]][[53]], digits = 3)     
p.s.m10b.f2.r     <-  zapsmall( s.m10b.f2[[10]][[53]], digits = 3)    
p.s.m100b.f2.r    <-  zapsmall( s.m100b.f2[[10]][[53]], digits = 3)   
p.s.m10000b.f2.r  <-  zapsmall( s.m10000b.f2[[10]][[53]], digits = 3) 
p.s.m1000b.f2.r   <-  zapsmall( s.m1000b.f2[[10]][[53]], digits = 3)  



p.s.m2.f2.r     <-  zapsmall( s.m2.f2[[10]][[54]], digits = 3)      
p.s.m20.f2.r    <-  zapsmall( s.m20.f2[[10]][[54]], digits = 3)     
p.s.m200.f2.r    <-  zapsmall( s.m200.f2[[10]][[54]], digits = 3)    
p.s.m20000.f2.r  <-  zapsmall( s.m20000.f2[[10]][[54]], digits = 3)  
p.s.m2000.f2.r  <-  zapsmall( s.m2000.f2[[10]][[54]], digits = 3)   



p.s.m3.f2.r     <-  zapsmall( s.m3.f2[[10]][[53]], digits = 3)      
# p.s.m30.f2.r    <-  zapsmall( s.m30.f2[[10]][[53]], digits = 3)     
p.s.m300.f2.r    <-  zapsmall( s.m300.f2[[10]][[53]], digits = 3)    
p.s.m30000.f2.r  <-  zapsmall( s.m30000.f2[[10]][[53]], digits = 3)  
p.s.m3000.f2.r   <-  zapsmall( s.m3000.f2[[10]][[53]], digits = 3)   



p.s.m3a.f2.r <-  zapsmall( s.m3a.f2[[10]][[53]], digits = 3)     
# p.s.m30a.f2.r <-  zapsmall( s.m30a.f2[[10]][[53]], digits = 3)    
p.s.m300a.f2.r <-  zapsmall( s.m300a.f2[[10]][[53]], digits = 3)   
p.s.m30000a.f2.r  <-  zapsmall( s.m30000a.f2[[10]][[53]], digits = 3) 
p.s.m3000a.f2.r  <-  zapsmall( s.m3000a.f2[[10]][[53]], digits = 3)  



p.s.m3b.f2.r <-  zapsmall( s.m3b.f2[[10]][[53]], digits = 3)     
# p.s.m30b.f2.r <-  zapsmall( s.m30b.f2[[10]][[53]], digits = 3)    
p.s.m300b.f2.r <-  zapsmall( s.m300b.f2[[10]][[53]], digits = 3)   
p.s.m30000b.f2.r  <-  zapsmall( s.m30000b.f2[[10]][[53]], digits = 3) 
p.s.m3000b.f2.r  <-  zapsmall( s.m3000b.f2[[10]][[53]], digits = 3)  



p.s.m1.f1.i <-  zapsmall( s.m1.f1[[10]][[54]], digits = 3)     
p.s.m10.f1.i <-  zapsmall( s.m10.f1[[10]][[54]], digits = 3)    
p.s.m100.f1.i <-  zapsmall( s.m100.f1[[10]][[54]], digits = 3)   
p.s.m10000.f1.i  <-  zapsmall( s.m10000.f1[[10]][[54]], digits = 3) 
p.s.m1000.f1.i <-  zapsmall( s.m1000.f1[[10]][[54]], digits = 3)  



p.s.m1a.f1.i <-  zapsmall( s.m1a.f1[[10]][[54]], digits = 3)    
p.s.m10a.f1.i <-  zapsmall( s.m10a.f1[[10]][[54]], digits = 3)   
p.s.m100a.f1.i <-  zapsmall( s.m100a.f1[[10]][[54]], digits = 3)  
p.s.m10000a.f1.i  <-  zapsmall( s.m10000a.f1[[10]][[54]], digits = 3)
p.s.m1000a.f1.i  <-  zapsmall( s.m1000a.f1[[10]][[54]], digits = 3) 



p.s.m1b.f1.i <-  zapsmall( s.m1b.f1[[10]][[54]], digits = 3)    
p.s.m10b.f1.i <-  zapsmall( s.m10b.f1[[10]][[54]], digits = 3)   
p.s.m100b.f1.i <-  zapsmall( s.m100b.f1[[10]][[54]], digits = 3)  
p.s.m10000b.f1.i  <-  zapsmall( s.m10000b.f1[[10]][[54]], digits = 3)
p.s.m1000b.f1.i  <-  zapsmall( s.m1000b.f1[[10]][[54]], digits = 3) 



p.s.m2.f1.i <-  zapsmall( s.m2.f1[[10]][[53]], digits = 3)     
# p.s.m20.f1.i <-  zapsmall( s.m20.f1[[10]][[54]], digits = 3)    
p.s.m200.f1.i <-  zapsmall( s.m200.f1[[10]][[53]], digits = 3)   
p.s.m20000.f1.i  <-  zapsmall( s.m20000.f1[[10]][[53]], digits = 3) 
p.s.m2000.f1.i <-  zapsmall( s.m2000.f1[[10]][[53]], digits = 3)  



p.s.m3.f1.i <-  zapsmall( s.m3.f1[[10]][[54]], digits = 3)     
# p.s.m30.f1.i <-  zapsmall( s.m30.f1[[10]][[54]], digits = 3)    
p.s.m300.f1.i <-  zapsmall( s.m300.f1[[10]][[54]], digits = 3)   
p.s.m30000.f1.i  <-  zapsmall( s.m30000.f1[[10]][[54]], digits = 3) 
p.s.m3000.f1.i <-  zapsmall( s.m3000.f1[[10]][[54]], digits = 3)  



p.s.m3a.f1.i <-  zapsmall( s.m3a.f1[[10]][[54]], digits = 3)    
# p.s.m30a.f1.i <-  zapsmall( s.m30a.f1[[10]][[54]], digits = 3)   
p.s.m300a.f1.i <-  zapsmall( s.m300a.f1[[10]][[54]], digits = 3)  
p.s.m30000a.f1.i  <-  zapsmall( s.m30000a.f1[[10]][[54]], digits = 3)
p.s.m3000a.f1.i  <-  zapsmall( s.m3000a.f1[[10]][[54]], digits = 3) 



p.s.m3b.f1.i <-  zapsmall( s.m3b.f1[[10]][[54]], digits = 3)    
# p.s.m30b.f1.i <-  zapsmall( s.m30b.f1[[10]][[54]], digits = 3)   
p.s.m300b.f1.i <-  zapsmall( s.m300b.f1[[10]][[54]], digits = 3)  
p.s.m30000b.f1.i  <-  zapsmall( s.m30000b.f1[[10]][[54]], digits = 3)
p.s.m3000b.f1.i  <-  zapsmall( s.m3000b.f1[[10]][[54]], digits = 3) 



p.s.m1.f2.i  <-  zapsmall( s.m1.f2[[10]][[54]], digits = 3)     
p.s.m10.f2.i  <-  zapsmall( s.m10.f2[[10]][[54]], digits = 3)    
p.s.m100.f2.i  <-  zapsmall( s.m100.f2[[10]][[54]], digits = 3)   
p.s.m10000.f2.i   <-  zapsmall( s.m10000.f2[[10]][[54]], digits = 3) 
p.s.m1000.f2.i  <-  zapsmall( s.m1000.f2[[10]][[54]], digits = 3)  



p.s.m1a.f2.i  <-  zapsmall( s.m1a.f2[[10]][[54]], digits = 3)    
p.s.m10a.f2.i  <-  zapsmall( s.m10a.f2[[10]][[54]], digits = 3)   
p.s.m100a.f2.i  <-  zapsmall( s.m100a.f2[[10]][[54]], digits = 3)  
p.s.m10000a.f2.i  <-  zapsmall( s.m10000a.f2[[10]][[54]], digits = 3)
p.s.m1000a.f2.i   <-  zapsmall( s.m1000a.f2[[10]][[54]], digits = 3) 



p.s.m1b.f2.i  <-  zapsmall( s.m1b.f2[[10]][[54]], digits = 3)    
p.s.m10b.f2.i  <-  zapsmall( s.m10b.f2[[10]][[54]], digits = 3)   
p.s.m100b.f2.i  <-  zapsmall( s.m100b.f2[[10]][[54]], digits = 3)  
p.s.m10000b.f2.i  <-  zapsmall( s.m10000b.f2[[10]][[54]], digits = 3)
p.s.m1000b.f2.i   <-  zapsmall( s.m1000b.f2[[10]][[54]], digits = 3) 



p.s.m2.f2.i  <-  zapsmall( s.m2.f2[[10]][[53]], digits = 3)     
# p.s.m20.f2.i  <-  zapsmall( s.m20.f2[[10]][[54]], digits = 3)    
p.s.m200.f2.i  <-  zapsmall( s.m200.f2[[10]][[53]], digits = 3)   
p.s.m20000.f2.i   <-  zapsmall( s.m20000.f2[[10]][[53]], digits = 3) 
p.s.m2000.f2.i  <-  zapsmall( s.m2000.f2[[10]][[53]], digits = 3)  



p.s.m3.f2.i  <-  zapsmall( s.m3.f2[[10]][[54]], digits = 3)     
# p.s.m30.f2.i  <-  zapsmall( s.m30.f2[[10]][[54]], digits = 3)    
p.s.m300.f2.i  <-  zapsmall( s.m300.f2[[10]][[54]], digits = 3)   
p.s.m30000.f2.i   <-  zapsmall( s.m30000.f2[[10]][[54]], digits = 3) 
p.s.m3000.f2.i  <-  zapsmall( s.m3000.f2[[10]][[54]], digits = 3)  



p.s.m3a.f2.i  <-  zapsmall( s.m3a.f2[[10]][[54]], digits = 3)    
# p.s.m30a.f2.i  <-  zapsmall( s.m30a.f2[[10]][[54]], digits = 3)   
p.s.m300a.f2.i  <-  zapsmall( s.m300a.f2[[10]][[54]], digits = 3)  
p.s.m30000a.f2.i  <-  zapsmall( s.m30000a.f2[[10]][[54]], digits = 3)
p.s.m3000a.f2.i   <-  zapsmall( s.m3000a.f2[[10]][[54]], digits = 3) 



p.s.m3b.f2.i  <-  zapsmall( s.m3b.f2[[10]][[54]], digits = 3)    
# p.s.m30b.f2.i  <-  zapsmall( s.m30b.f2[[10]][[54]], digits = 3)   
p.s.m300b.f2.i  <-  zapsmall( s.m300b.f2[[10]][[54]], digits = 3)  
p.s.m30000b.f2.i  <-  zapsmall( s.m30000b.f2[[10]][[54]], digits = 3)
p.s.m3000b.f2.i   <-  zapsmall( s.m3000b.f2[[10]][[54]], digits = 3) 



p.s.m1.f1.ri <-  zapsmall( s.m1.f1[[10]][[55]], digits = 3)     
p.s.m10.f1.ri <-  zapsmall( s.m10.f1[[10]][[55]], digits = 3)    
p.s.m100.f1.ri <-  zapsmall( s.m100.f1[[10]][[55]], digits = 3)   
p.s.m10000.f1.ri  <-  zapsmall( s.m10000.f1[[10]][[55]], digits = 3) 
p.s.m1000.f1.ri <-  zapsmall( s.m1000.f1[[10]][[55]], digits = 3)  



p.s.m1a.f1.ri <-  zapsmall( s.m1a.f1[[10]][[55]], digits = 3)    
p.s.m10a.f1.ri <-  zapsmall( s.m10a.f1[[10]][[55]], digits = 3)   
p.s.m100a.f1.ri <-  zapsmall( s.m100a.f1[[10]][[55]], digits = 3)  
p.s.m10000a.f1.ri  <-  zapsmall( s.m10000a.f1[[10]][[55]], digits = 3)
p.s.m1000a.f1.ri  <-  zapsmall( s.m1000a.f1[[10]][[55]], digits = 3) 



p.s.m1b.f1.ri     <-  zapsmall( s.m1b.f1[[10]][[55]], digits = 3)    
p.s.m10b.f1.ri    <-  zapsmall( s.m10b.f1[[10]][[55]], digits = 3)   
p.s.m100b.f1.ri   <-  zapsmall( s.m100b.f1[[10]][[55]], digits = 3)  
p.s.m10000b.f1.ri     <-  zapsmall( s.m10000b.f1[[10]][[55]], digits = 3)
p.s.m1000b.f1.ri  <-  zapsmall( s.m1000b.f1[[10]][[55]], digits = 3) 



p.s.m2.f1.ri       <-  zapsmall( s.m2.f1[[10]][[55]], digits = 3)     
# p.s.m20.f1.ri      <-  zapsmall( s.m20.f1[[10]][[55]], digits = 3)    
p.s.m200.f1.ri     <-  zapsmall( s.m200.f1[[10]][[55]], digits = 3)   
p.s.m20000.f1.ri   <-  zapsmall( s.m20000.f1[[10]][[55]], digits = 3) 
p.s.m2000.f1.ri    <-  zapsmall( s.m2000.f1[[10]][[55]], digits = 3)  



p.s.m3.f1.ri      <-  zapsmall( s.m3.f1[[10]][[55]], digits = 3)     
# p.s.m30.f1.ri     <-  zapsmall( s.m30.f1[[10]][[55]], digits = 3)    
p.s.m300.f1.ri    <-  zapsmall( s.m300.f1[[10]][[55]], digits = 3)   
p.s.m30000.f1.ri  <-  zapsmall( s.m30000.f1[[10]][[55]], digits = 3) 
p.s.m3000.f1.ri   <-  zapsmall( s.m3000.f1[[10]][[55]], digits = 3)  



p.s.m3a.f1.ri      <-  zapsmall( s.m3a.f1[[10]][[55]], digits = 3)    
# p.s.m30a.f1.ri     <-  zapsmall( s.m30a.f1[[10]][[55]], digits = 3)   
p.s.m300a.f1.ri     <-  zapsmall( s.m300a.f1[[10]][[55]], digits = 3) 
p.s.m30000a.f1.ri      <-  zapsmall( s.m30000a.f1[[10]][[55]], digits = 3)
p.s.m3000a.f1.ri     <-  zapsmall( s.m3000a.f1[[10]][[55]], digits = 3) 



p.s.m3b.f1.ri <-  zapsmall( s.m3b.f1[[10]][[55]], digits = 3)    
# p.s.m30b.f1.ri <-  zapsmall( s.m30b.f1[[10]][[55]], digits = 3)   
p.s.m300b.f1.ri <-  zapsmall( s.m300b.f1[[10]][[55]], digits = 3)  
p.s.m30000b.f1.ri  <-  zapsmall( s.m30000b.f1[[10]][[55]], digits = 3)
p.s.m3000b.f1.ri  <-  zapsmall( s.m3000b.f1[[10]][[55]], digits = 3) 



p.s.m1.f2.ri       <-  zapsmall( s.m1.f2[[10]][[55]], digits = 3)     
p.s.m10.f2.ri      <-  zapsmall( s.m10.f2[[10]][[55]], digits = 3)    
p.s.m100.f2.ri     <-  zapsmall( s.m100.f2[[10]][[55]], digits = 3)   
p.s.m10000.f2.ri   <-  zapsmall( s.m10000.f2[[10]][[55]], digits = 3) 
p.s.m1000.f2.ri    <-  zapsmall( s.m1000.f2[[10]][[55]], digits = 3)  



p.s.m1a.f2.ri      <-  zapsmall( s.m1a.f2[[10]][[55]], digits = 3)    
p.s.m10a.f2.ri    <-  zapsmall( s.m10a.f2[[10]][[55]], digits = 3)   
p.s.m100a.f2.ri    <-  zapsmall( s.m100a.f2[[10]][[55]], digits = 3)  
p.s.m10000a.f2.ri      <-  zapsmall( s.m10000a.f2[[10]][[55]], digits = 3)
p.s.m1000a.f2.ri   <-  zapsmall( s.m1000a.f2[[10]][[55]], digits = 3) 



p.s.m1b.f2.ri <-  zapsmall( s.m1b.f2[[10]][[55]], digits = 3)    
p.s.m10b.f2.ri <-  zapsmall( s.m10b.f2[[10]][[55]], digits = 3)   
p.s.m100b.f2.ri <-  zapsmall( s.m100b.f2[[10]][[55]], digits = 3)  
p.s.m10000b.f2.ri  <-  zapsmall( s.m10000b.f2[[10]][[55]], digits = 3)
p.s.m1000b.f2.ri  <-  zapsmall( s.m1000b.f2[[10]][[55]], digits = 3) 



p.s.m2.f2.ri <-  zapsmall( s.m2.f2[[10]][[55]], digits = 3)     
# p.s.m20.f2.ri <-  zapsmall( s.m20.f2[[10]][[55]], digits = 3)    
p.s.m200.f2.ri <-  zapsmall( s.m200.f2[[10]][[55]], digits = 3)   
p.s.m20000.f2.ri  <-  zapsmall( s.m20000.f2[[10]][[55]], digits = 3) 
p.s.m2000.f2.ri <-  zapsmall( s.m2000.f2[[10]][[55]], digits = 3)  



p.s.m3.f2.ri <-  zapsmall( s.m3.f2[[10]][[55]], digits = 3)     
# p.s.m30.f2.ri <-  zapsmall( s.m30.f2[[10]][[55]], digits = 3)    
p.s.m300.f2.ri <-  zapsmall( s.m300.f2[[10]][[55]], digits = 3)   
p.s.m30000.f2.ri  <-  zapsmall( s.m30000.f2[[10]][[55]], digits = 3) 
p.s.m3000.f2.ri <-  zapsmall( s.m3000.f2[[10]][[55]], digits = 3)  



p.s.m3a.f2.ri <-  zapsmall( s.m3a.f2[[10]][[55]], digits = 3)    
# p.s.m30a.f2.ri <-  zapsmall( s.m30a.f2[[10]][[55]], digits = 3)   
p.s.m300a.f2.ri <-  zapsmall( s.m300a.f2[[10]][[55]], digits = 3)  
p.s.m30000a.f2.ri  <-  zapsmall( s.m30000a.f2[[10]][[55]], digits = 3)
p.s.m3000a.f2.ri  <-  zapsmall( s.m3000a.f2[[10]][[55]], digits = 3) 



p.s.m3b.f2.ri <-  zapsmall( s.m3b.f2[[10]][[55]], digits = 3)    
# p.s.m30b.f2.ri <-  zapsmall( s.m30b.f2[[10]][[55]], digits = 3)   
p.s.m300b.f2.ri <-  zapsmall( s.m300b.f2[[10]][[55]], digits = 3)  
p.s.m30000b.f2.ri  <-  zapsmall( s.m30000b.f2[[10]][[55]], digits = 3)
p.s.m3000b.f2.ri  <-  zapsmall( s.m3000b.f2[[10]][[55]], digits = 3)   