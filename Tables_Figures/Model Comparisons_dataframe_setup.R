
###############################################
#  F1 
###############################################

# -------------------- SEMREL: SET UP INPUT FILE(S) -----------------------------------
f1                 <- read.csv("data/f1_critical_items.csv")
f1$X.1             = NULL
f1$X               = NULL
# -------------------- SEMREL: PREPARE NUISANCE/PREDICTOR VARIABLES FOR SEMREL ---------
f1                  <- subset( f1, subexpt == "SemRel")
f1$subject          <- as.factor( f1$subject)
f1$subject          <- droplevels( f1$subject)

f1$len.char.ns      <- rowSums( f1[ ,  12:15])
f1$len.char         <- scale( f1$len.char.ns, center=TRUE, scale=TRUE)
f1$LengthChar.Head  = NULL
f1$LengthChar.Prep  = NULL
f1$LengthChar.Adj   = NULL
f1$LengthChar.Noun  = NULL
f1$len.phon.ns      <- rowSums( f1[ , 12:15])
f1$len.phon         <- scale( f1$len.phon.ns, center=TRUE, scale=TRUE)
f1$LengthPhon.Head  = NULL
f1$LengthPhon.Prep  = NULL
f1$LengthPhon.Adj   = NULL
f1$LengthPhon.Noun  = NULL
f1$len.syll.ns      <- rowSums( f1[ , 12:15])  
f1$len.syll         <- scale( f1$len.syll.ns, center=TRUE, scale=TRUE)
f1$LengthSylHead    = NULL
f1$LengthSyll.Prep  = NULL
f1$LengthSyll.Adj   = NULL
f1$LengthSyll.Noun  = NULL
f1$assHL            <- rowSums( f1[ , 24:25])
f1$assHL.asin.ns    <- 2 * asin( sqrt( f1$assHL / 100))
f1$assHL.asin       <- scale( f1$assHL.asin.ns,  center = TRUE, scale = TRUE)
f1$AssPercent.HL.S  = NULL
f1$AssPercent.HL.P  = NULL
f1$LNSing.HS        = NULL
f1$LNSing.HP        = NULL   
f1$LNPlur.HS        = NULL   
f1$LNPlur.HP        = NULL   
f1$Freq1Mil.Head    = NULL
f1$FreqMil.Adj      = NULL
f1$FreqMil.Prep     = NULL
f1$FreqMil.N1       = NULL
f1$RelatedLH        = NULL
f1$lf.head          <- scale( f1$LogFr.Head,   center = TRUE, scale = TRUE)
f1$lf.prep          <- scale( f1$LogFreq.Prep, center = TRUE, scale = TRUE)
f1$lf.adj           <- scale( f1$LogFreq.Adj,  center = TRUE, scale = TRUE)
f1$lf.noun          <- scale( f1$LogFreq.N1,   center = TRUE, scale = TRUE)
f1$rel              <- scale( f1$RelatedHL,    center = TRUE, scale = TRUE)
f1$int              <- scale( f1$Integrated,   center = TRUE, scale = TRUE)
f1$plaus            <- scale( f1$Plausibility, center = TRUE, scale = TRUE)
f1$LogFr.Head       = NULL
f1$LogFreq.Prep     = NULL
f1$LogFreq.Adj      = NULL
f1$LogFreq.N1       = NULL
f1$RelatedHL        = NULL
f1$RelatedLH        = NULL
f1$Integrated       = NULL
f1$Plausibility     = NULL
f1                  <- f1[ , c(1:12,32:56)]
f1$len.char.ns      = NULL
f1$len.phon.ns      = NULL
f1$len.syll.ns      = NULL
f1$assHL.asin.ns    = NULL
f1$animacy          = NULL 
f1$all.asso         = NULL
f1$local.asso       = NULL
f1$assHL            = NULL 
f1$assHL           <- f1$assHL.asin
f1$assHL.asin       = NULL   
f1$res.int          <-scale( f1$res.int, center = TRUE, scale = TRUE)
f1$res.rel          <-scale( f1$res.rel, center = TRUE, scale = TRUE)
f1$res.asshl        <-scale( f1$res.asshl, center = TRUE, scale = TRUE)
f1$res.rel2         <-scale( f1$res.rel2, center = TRUE, scale = TRUE)

f1$corr             = f1$errcord - f1$errd 
f1$tot              = f1$errcord  
f1$elog             = log( ( f1$errd + .5) / ( f1$corr+.5) )  
f1$rates            = ifelse ( f1$errd == 0 & f1$corr == 0, 0, ( f1$errd / f1$tot) * 100) 
f1$v                = (1 / ( f1$errd + .5)) + (1 / ( f1$corr + .5)) 
f1$asin             = asin(sqrt ( f1$rates / 100))  
f1.sr               <- f1
rm( f1)
# -------------------- CAT & PROP: SET UP INPUT FILE(S) --------------------------------
f1                 <- read.csv("data/f1_critical_items.csv")
f1$X.1             = NULL
f1$X               = NULL
# -------------------- CAT & PROP: PREPARE NUISANCE/PREDICTOR VARIABLES 
f1                  <- subset( f1, subexpt != "SemRel")
f1$subject          <- as.factor( f1$subject)
f1$subject          <- droplevels( f1$subject)

f1$len.char.ns      <- rowSums( f1[ ,  12:15])
f1$len.char         <- scale( f1$len.char.ns, center=TRUE, scale=TRUE)
f1$LengthChar.Head  = NULL
f1$LengthChar.Prep  = NULL
f1$LengthChar.Adj   = NULL
f1$LengthChar.Noun  = NULL
f1$len.phon.ns      <- rowSums( f1[ , 12:15])
f1$len.phon         <- scale( f1$len.phon.ns, center=TRUE, scale=TRUE)
f1$LengthPhon.Head  = NULL
f1$LengthPhon.Prep  = NULL
f1$LengthPhon.Adj   = NULL
f1$LengthPhon.Noun  = NULL
f1$len.syll.ns      <- rowSums( f1[ , 12:15])  
f1$len.syll         <- scale( f1$len.syll.ns, center=TRUE, scale=TRUE)
f1$LengthSylHead    = NULL
f1$LengthSyll.Prep  = NULL
f1$LengthSyll.Adj   = NULL
f1$LengthSyll.Noun  = NULL
f1$assHL            <- rowSums( f1[ , 24:25])
f1$assHL.asin.ns    <- 2 * asin( sqrt( f1$assHL / 100))
f1$assHL.asin       <- scale( f1$assHL.asin.ns,  center = TRUE, scale = TRUE)
f1$AssPercent.HL.S  = NULL
f1$AssPercent.HL.P  = NULL
f1$LNSing.HS        = NULL
f1$LNSing.HP        = NULL   
f1$LNPlur.HS        = NULL   
f1$LNPlur.HP        = NULL   
f1$Freq1Mil.Head    = NULL
f1$FreqMil.Adj      = NULL
f1$FreqMil.Prep     = NULL
f1$FreqMil.N1       = NULL
f1$RelatedLH        = NULL
f1$lf.head          <- scale( f1$LogFr.Head,   center = TRUE, scale = TRUE)
f1$lf.prep          <- scale( f1$LogFreq.Prep, center = TRUE, scale = TRUE)
f1$lf.adj           <- scale( f1$LogFreq.Adj,  center = TRUE, scale = TRUE)
f1$lf.noun          <- scale( f1$LogFreq.N1,   center = TRUE, scale = TRUE)
f1$rel              <- scale( f1$RelatedHL,    center = TRUE, scale = TRUE)
f1$int              <- scale( f1$Integrated,   center = TRUE, scale = TRUE)
f1$plaus            <- scale( f1$Plausibility, center = TRUE, scale = TRUE)
f1$LogFr.Head       = NULL
f1$LogFreq.Prep     = NULL
f1$LogFreq.Adj      = NULL
f1$LogFreq.N1       = NULL
f1$RelatedHL        = NULL
f1$RelatedLH        = NULL
f1$Integrated       = NULL
f1$Plausibility     = NULL
f1                  <- f1[ , c(1:12,32:56)]
f1$len.char.ns      = NULL
f1$len.phon.ns      = NULL
f1$len.syll.ns      = NULL
f1$assHL.asin.ns    = NULL
f1$animacy          = NULL 
f1$all.asso         = NULL
f1$local.asso       = NULL
f1$assHL            = NULL 
f1$assHL           <- f1$assHL.asin
f1$assHL.asin       = NULL  
f1$res.int          <-scale( f1$res.int, center = TRUE, scale = TRUE)
f1$res.rel          <-scale( f1$res.rel, center = TRUE, scale = TRUE)
f1$res.asshl        <-scale( f1$res.asshl, center = TRUE, scale = TRUE)
f1$res.rel2         <-scale( f1$res.rel2, center = TRUE, scale = TRUE)

f1$corr             = f1$errcord - f1$errd 
f1$tot              = f1$errcord  
f1$elog             = log( ( f1$errd + .5) / ( f1$corr+.5) )  
f1$rates            = ifelse ( f1$errd == 0 & f1$corr == 0, 0, ( f1$errd / f1$tot) * 100) 
f1$v                = (1 / ( f1$errd + .5)) + (1 / ( f1$corr + .5)) 
f1$asin             = asin(sqrt ( f1$rates / 100))  

f1.cc         <- subset( f1, subexpt == "Cat")
f1.cc$subject <- as.factor( f1.cc$subject)
f1.cc$subject <- droplevels( f1.cc$subject)
f1.pr         <- subset( f1, subexpt == "Prop")
f1.pr$subject <- as.factor( f1.pr$subject)
f1.pr$subject <- droplevels( f1.pr$subject)


# -------------------- ALL ITEMS: SET UP INPUT FILE(S) --------------------------------
f1                 <- read.csv("data/f1_critical_items.csv")
f1$X.1             = NULL
f1$X               = NULL
# -------------------- ALL ITEMS: PREPARE NUISANCE/PREDICTOR VARIABLES FOR ALL -------
f1$subject          <- as.factor( f1$subject)
f1$subject          <- droplevels( f1$subject)

f1$len.char.ns      <- rowSums( f1[ ,  12:15])
f1$len.char         <- scale( f1$len.char.ns, center=TRUE, scale=TRUE)
f1$LengthChar.Head  = NULL
f1$LengthChar.Prep  = NULL
f1$LengthChar.Adj   = NULL
f1$LengthChar.Noun  = NULL
f1$len.phon.ns      <- rowSums( f1[ , 12:15])
f1$len.phon         <- scale( f1$len.phon.ns, center=TRUE, scale=TRUE)
f1$LengthPhon.Head  = NULL
f1$LengthPhon.Prep  = NULL
f1$LengthPhon.Adj   = NULL
f1$LengthPhon.Noun  = NULL
f1$len.syll.ns      <- rowSums( f1[ , 12:15])  
f1$len.syll         <- scale( f1$len.syll.ns, center=TRUE, scale=TRUE)
f1$LengthSylHead    = NULL
f1$LengthSyll.Prep  = NULL
f1$LengthSyll.Adj   = NULL
f1$LengthSyll.Noun  = NULL
f1$assHL            <- rowSums( f1[ , 24:25])
f1$assHL.asin.ns    <- 2 * asin( sqrt( f1$assHL / 100))
f1$assHL.asin       <- scale( f1$assHL.asin.ns,  center = TRUE, scale = TRUE)
f1$AssPercent.HL.S  = NULL
f1$AssPercent.HL.P  = NULL
f1$LNSing.HS        = NULL
f1$LNSing.HP        = NULL   
f1$LNPlur.HS        = NULL   
f1$LNPlur.HP        = NULL   
f1$Freq1Mil.Head    = NULL
f1$FreqMil.Adj      = NULL
f1$FreqMil.Prep     = NULL
f1$FreqMil.N1       = NULL
f1$RelatedLH        = NULL
f1$lf.head          <- scale( f1$LogFr.Head,   center = TRUE, scale = TRUE)
f1$lf.prep          <- scale( f1$LogFreq.Prep, center = TRUE, scale = TRUE)
f1$lf.adj           <- scale( f1$LogFreq.Adj,  center = TRUE, scale = TRUE)
f1$lf.noun          <- scale( f1$LogFreq.N1,   center = TRUE, scale = TRUE)
f1$rel              <- scale( f1$RelatedHL,    center = TRUE, scale = TRUE)
f1$int              <- scale( f1$Integrated,   center = TRUE, scale = TRUE)
f1$plaus            <- scale( f1$Plausibility, center = TRUE, scale = TRUE)
f1$LogFr.Head       = NULL
f1$LogFreq.Prep     = NULL
f1$LogFreq.Adj      = NULL
f1$LogFreq.N1       = NULL
f1$RelatedHL        = NULL
f1$RelatedLH        = NULL
f1$Integrated       = NULL
f1$Plausibility     = NULL
f1                  <- f1[ , c(1:12,32:56)]
f1$len.char.ns      = NULL
f1$len.phon.ns      = NULL
f1$len.syll.ns      = NULL
f1$assHL.asin.ns    = NULL
f1$animacy          = NULL 
f1$all.asso         = NULL
f1$local.asso       = NULL
f1$assHL            = NULL 
f1$assHL           <- f1$assHL.asin
f1$assHL.asin       = NULL
f1$res.int          <-scale( f1$res.int, center = TRUE, scale = TRUE)
f1$res.rel          <-scale( f1$res.rel, center = TRUE, scale = TRUE)
f1$res.asshl        <-scale( f1$res.asshl, center = TRUE, scale = TRUE)
f1$res.rel2         <-scale( f1$res.rel2, center = TRUE, scale = TRUE)

f1$corr             = f1$errcord - f1$errd 
f1$tot              = f1$errcord  
f1$elog             = log( ( f1$errd + .5) / ( f1$corr+.5) )  
f1$rates            = ifelse ( f1$errd == 0 & f1$corr == 0, 0, ( f1$errd / f1$tot) * 100) 
f1$v                = (1 / ( f1$errd + .5)) + (1 / ( f1$corr + .5)) 
f1$asin             = asin(sqrt ( f1$rates / 100))  
f1.all               <- f1

# -------------------- SEMREl 1 & PROP ITEMS: SET UP INPUT FILE(S) --------------------------------

f1                 <- read.csv("data/f1_critical_items.csv")
f1$X.1             = NULL
f1$X               = NULL
# -------------------- SEMREl 1 & PROP: PREPARE NUISANCE/PREDICTOR VARIABLES FOR ALL --------------
f1                  <- subset( f1, subexpt != "Cat")
f1$subject          <- as.factor( f1$subject)
f1$subject          <- droplevels( f1$subject)

f1$len.char.ns      <- rowSums( f1[ ,  12:15])
f1$len.char         <- scale( f1$len.char.ns, center=TRUE, scale=TRUE)
f1$LengthChar.Head  = NULL
f1$LengthChar.Prep  = NULL
f1$LengthChar.Adj   = NULL
f1$LengthChar.Noun  = NULL
f1$len.phon.ns      <- rowSums( f1[ , 12:15])
f1$len.phon         <- scale( f1$len.phon.ns, center=TRUE, scale=TRUE)
f1$LengthPhon.Head  = NULL
f1$LengthPhon.Prep  = NULL
f1$LengthPhon.Adj   = NULL
f1$LengthPhon.Noun  = NULL
f1$len.syll.ns      <- rowSums( f1[ , 12:15])  
f1$len.syll         <- scale( f1$len.syll.ns, center=TRUE, scale=TRUE)
f1$LengthSylHead    = NULL
f1$LengthSyll.Prep  = NULL
f1$LengthSyll.Adj   = NULL
f1$LengthSyll.Noun  = NULL
f1$assHL            <- rowSums( f1[ , 24:25])
f1$assHL.asin.ns    <- 2 * asin( sqrt( f1$assHL / 100))
f1$assHL.asin       <- scale( f1$assHL.asin.ns,  center = TRUE, scale = TRUE)
f1$AssPercent.HL.S  = NULL
f1$AssPercent.HL.P  = NULL
f1$LNSing.HS        = NULL
f1$LNSing.HP        = NULL   
f1$LNPlur.HS        = NULL   
f1$LNPlur.HP        = NULL   
f1$Freq1Mil.Head    = NULL
f1$FreqMil.Adj      = NULL
f1$FreqMil.Prep     = NULL
f1$FreqMil.N1       = NULL
f1$RelatedLH        = NULL
f1$lf.head          <- scale( f1$LogFr.Head,   center = TRUE, scale = TRUE)
f1$lf.prep          <- scale( f1$LogFreq.Prep, center = TRUE, scale = TRUE)
f1$lf.adj           <- scale( f1$LogFreq.Adj,  center = TRUE, scale = TRUE)
f1$lf.noun          <- scale( f1$LogFreq.N1,   center = TRUE, scale = TRUE)
f1$rel              <- scale( f1$RelatedHL,    center = TRUE, scale = TRUE)
f1$int              <- scale( f1$Integrated,   center = TRUE, scale = TRUE)
f1$plaus            <- scale( f1$Plausibility, center = TRUE, scale = TRUE)
f1$LogFr.Head       = NULL
f1$LogFreq.Prep     = NULL
f1$LogFreq.Adj      = NULL
f1$LogFreq.N1       = NULL
f1$RelatedHL        = NULL
f1$RelatedLH        = NULL
f1$Integrated       = NULL
f1$Plausibility     = NULL
f1                  <- f1[ , c(1:12,32:56)]
f1$len.char.ns      = NULL
f1$len.phon.ns      = NULL
f1$len.syll.ns      = NULL
f1$assHL.asin.ns    = NULL
f1$animacy          = NULL 
f1$all.asso         = NULL
f1$local.asso       = NULL
f1$assHL            = NULL 
f1$assHL           <- f1$assHL.asin
f1$assHL.asin       = NULL      
f1$res.int          <-scale( f1$res.int, center = TRUE, scale = TRUE)
f1$res.rel          <-scale( f1$res.rel, center = TRUE, scale = TRUE)
f1$res.asshl        <-scale( f1$res.asshl, center = TRUE, scale = TRUE)
f1$res.rel2         <-scale( f1$res.rel2, center = TRUE, scale = TRUE)

f1$corr             = f1$errcord - f1$errd 
f1$tot              = f1$errcord  
f1$elog             = log( ( f1$errd + .5) / ( f1$corr+.5) )  
f1$rates            = ifelse ( f1$errd == 0 & f1$corr == 0, 0, ( f1$errd / f1$tot) * 100) 
f1$v                = (1 / ( f1$errd + .5)) + (1 / ( f1$corr + .5)) 
f1$asin             = asin(sqrt ( f1$rates / 100))  
f1.sr.prop                <- f1
rm( f1)


################################################
#  F2 
###############################################


# -------------------- SEMREL: SET UP INPUT FILE(S) -----------------------------------
f2                 <- read.csv("data/f2_critical_items.csv")
f2$X                = NULL
f2$X.1               = NULL

# -------------------- SEMREL: PREPARE NUISANCE/PREDICTOR VARIABLES FOR SEMREL ---------
f2                  <- subset( f2, subexpt == "SemRel")
f2$item             <- as.factor( f2$item)
f2$item             <- droplevels( f2$item)

f2$len.char.ns      <- rowSums( f2[ ,  9:12])
f2$len.char         <- scale( f2$len.char.ns, center=TRUE, scale=TRUE)
f2$LengthChar.Head  = NULL
f2$LengthChar.Prep  = NULL
f2$LengthChar.Adj   = NULL
f2$LengthChar.Noun  = NULL
f2$len.phon.ns      <- rowSums( f2[ , 9:12])
f2$len.phon         <- scale( f2$len.phon.ns, center=TRUE, scale=TRUE)
f2$LengthPhon.Head  = NULL
f2$LengthPhon.Prep  = NULL
f2$LengthPhon.Adj   = NULL
f2$LengthPhon.Noun  = NULL
f2$len.syll.ns      <- rowSums( f2[ , 9:12])  
f2$len.syll         <- scale( f2$len.syll.ns, center=TRUE, scale=TRUE)
f2$LengthSylHead    = NULL
f2$LengthSyll.Prep  = NULL
f2$LengthSyll.Adj   = NULL
f2$LengthSyll.Noun  = NULL
f2$assHL            <- rowSums( f2[ , 25:26])
f2$assHL.asin.ns    <- 2 * asin( sqrt( f2$assHL / 100))
f2$assHL.asin       <- scale( f2$assHL.asin.ns,  center = TRUE, scale = TRUE)
f2$AssPercent.HL.S  = NULL
f2$AssPercent.HL.P  = NULL
f2$LNSing.HS        = NULL
f2$LNSing.HP        = NULL   
f2$LNPlur.HS        = NULL   
f2$LNPlur.HP        = NULL   
f2$Freq1Mil.Head    = NULL
f2$FreqMil.Adj      = NULL
f2$FreqMil.Prep     = NULL
f2$FreqMil.N1       = NULL
f2$RelatedLH        = NULL
f2$lf.head          <- scale( f2$LogFr.Head,   center = TRUE, scale = TRUE)
f2$lf.prep          <- scale( f2$LogFreq.Prep, center = TRUE, scale = TRUE)
f2$lf.adj           <- scale( f2$LogFreq.Adj,  center = TRUE, scale = TRUE)
f2$lf.noun          <- scale( f2$LogFreq.N1,   center = TRUE, scale = TRUE)
f2$rel              <- scale( f2$RelatedHL,    center = TRUE, scale = TRUE)
f2$int              <- scale( f2$Integrated,   center = TRUE, scale = TRUE)
f2$plaus            <- scale( f2$Plausibility, center = TRUE, scale = TRUE)
f2$LogFr.Head       = NULL
f2$LogFreq.Prep     = NULL
f2$LogFreq.Adj      = NULL
f2$LogFreq.N1       = NULL
f2$RelatedHL        = NULL
f2$RelatedLH        = NULL
f2$Integrated       = NULL
f2$Plausibility     = NULL

f2                  <- f2[ , c(1:12,32:53)]
f2$len.char.ns      = NULL
f2$len.phon.ns      = NULL
f2$len.syll.ns      = NULL
f2$assHL.asin.ns    = NULL
f2$animacy          = NULL 
f2$all.asso         = NULL
f2$local.asso       = NULL
f2$assHL            = NULL 
f2$assHL           <- f2$assHL.asin
f2$assHL.asin       = NULL 
f2$res.int          <-scale( f2$res.int, center = TRUE, scale = TRUE)
f2$res.rel          <-scale( f2$res.rel, center = TRUE, scale = TRUE)
f2$res.asshl        <-scale( f2$res.asshl, center = TRUE, scale = TRUE)
f2$res.rel2         <-scale( f2$res.rel2, center = TRUE, scale = TRUE)

f2$corr             = f2$errcord - f2$errd 
f2$tot              = f2$errcord  
f2$elog             = log( ( f2$errd + .5) / ( f2$corr+.5) )  
f2$rates            = ifelse ( f2$errd == 0 & f2$corr == 0, 0, ( f2$errd / f2$tot) * 100) 
f2$v                = (1 / ( f2$errd + .5)) + (1 / ( f2$corr + .5)) 
f2$asin             = asin(sqrt ( f2$rates / 100))  
f2.sr               <- f2

# -------------------- CAT & PROP: SET UP INPUT FILE(S) --------------------------------
f2           <- read.csv("data/f2_critical_items.csv")
f2$X                = NULL
f2$X.1               = NULL
# -------------------- CAT & PROP: PREPARE NUISANCE/PREDICTOR VARIABLES

f2                  <- subset( f2, subexpt != "SemRel")
f2$item             <- as.factor( f2$item)
f2$item             <- droplevels( f2$item)

f2$len.char.ns      <- rowSums( f2[ ,  9:12])
f2$len.char         <- scale( f2$len.char.ns, center=TRUE, scale=TRUE)
f2$LengthChar.Head  = NULL
f2$LengthChar.Prep  = NULL
f2$LengthChar.Adj   = NULL
f2$LengthChar.Noun  = NULL
f2$len.phon.ns      <- rowSums( f2[ , 9:12])
f2$len.phon         <- scale( f2$len.phon.ns, center=TRUE, scale=TRUE)
f2$LengthPhon.Head  = NULL
f2$LengthPhon.Prep  = NULL
f2$LengthPhon.Adj   = NULL
f2$LengthPhon.Noun  = NULL
f2$len.syll.ns      <- rowSums( f2[ , 9:12])  
f2$len.syll         <- scale( f2$len.syll.ns, center=TRUE, scale=TRUE)
f2$LengthSylHead    = NULL
f2$LengthSyll.Prep  = NULL
f2$LengthSyll.Adj   = NULL
f2$LengthSyll.Noun  = NULL
f2$assHL            <- rowSums( f2[ , 25:26])
f2$assHL.asin.ns    <- 2 * asin( sqrt( f2$assHL / 100))
f2$assHL.asin       <- scale( f2$assHL.asin.ns,  center = TRUE, scale = TRUE)
f2$AssPercent.HL.S  = NULL
f2$AssPercent.HL.P  = NULL
f2$LNSing.HS        = NULL
f2$LNSing.HP        = NULL   
f2$LNPlur.HS        = NULL   
f2$LNPlur.HP        = NULL   
f2$Freq1Mil.Head    = NULL
f2$FreqMil.Adj      = NULL
f2$FreqMil.Prep     = NULL
f2$FreqMil.N1       = NULL
f2$RelatedLH        = NULL
f2$lf.head          <- scale( f2$LogFr.Head,   center = TRUE, scale = TRUE)
f2$lf.prep          <- scale( f2$LogFreq.Prep, center = TRUE, scale = TRUE)
f2$lf.adj           <- scale( f2$LogFreq.Adj,  center = TRUE, scale = TRUE)
f2$lf.noun          <- scale( f2$LogFreq.N1,   center = TRUE, scale = TRUE)
f2$rel              <- scale( f2$RelatedHL,    center = TRUE, scale = TRUE)
f2$int              <- scale( f2$Integrated,   center = TRUE, scale = TRUE)
f2$plaus            <- scale( f2$Plausibility, center = TRUE, scale = TRUE)
f2$LogFr.Head       = NULL
f2$LogFreq.Prep     = NULL
f2$LogFreq.Adj      = NULL
f2$LogFreq.N1       = NULL
f2$RelatedHL        = NULL
f2$RelatedLH        = NULL
f2$Integrated       = NULL
f2$Plausibility     = NULL

f2                  <- f2[ , c(1:12,32:53)]
f2$len.char.ns      = NULL
f2$len.phon.ns      = NULL
f2$len.syll.ns      = NULL
f2$assHL.asin.ns    = NULL
f2$animacy          = NULL 
f2$all.asso         = NULL
f2$local.asso       = NULL
f2$assHL            = NULL 
f2$assHL           <- f2$assHL.asin
f2$assHL.asin       = NULL  
f2$res.int          <-scale( f2$res.int, center = TRUE, scale = TRUE)
f2$res.rel          <-scale( f2$res.rel, center = TRUE, scale = TRUE)
f2$res.asshl        <-scale( f2$res.asshl, center = TRUE, scale = TRUE)
f2$res.rel2         <-scale( f2$res.rel2, center = TRUE, scale = TRUE)

f2$corr             = f2$errcord - f2$errd 
f2$tot              = f2$errcord  
f2$elog             = log( ( f2$errd + .5) / ( f2$corr+.5) )  
f2$rates            = ifelse ( f2$errd == 0 & f2$corr == 0, 0, ( f2$errd / f2$tot) * 100) 
f2$v                = (1 / ( f2$errd + .5)) + (1 / ( f2$corr + .5)) 
f2$asin             = asin(sqrt ( f2$rates / 100))  
f2.cc              <- subset( f2, subexpt == "Cat")

f2.cc         <- subset( f2, subexpt == "Cat")
f2.cc$item <- as.factor( f2.cc$item)
f2.cc$item <- droplevels( f2.cc$item)
f2.pr         <- subset( f2, subexpt == "Prop")
f2.pr$item <- as.factor( f2.pr$item)
f2.pr$item <- droplevels( f2.pr$item)


# -------------------- ALL ITEMS: SET UP INPUT FILE(S) --------------------------------
f2                 <- read.csv("data/f2_critical_items.csv")
f2$X                = NULL
f2$X.1               = NULL
# -------------------- ALL ITEMS: PREPARE NUISANCE/PREDICTOR VARIABLES FOR ALL -------
f2$item             <- as.factor( f2$item)
f2$item             <- droplevels( f2$item)

f2$len.char.ns      <- rowSums( f2[ ,  9:12])
f2$len.char         <- scale( f2$len.char.ns, center=TRUE, scale=TRUE)
f2$LengthChar.Head  = NULL
f2$LengthChar.Prep  = NULL
f2$LengthChar.Adj   = NULL
f2$LengthChar.Noun  = NULL
f2$len.phon.ns      <- rowSums( f2[ , 9:12])
f2$len.phon         <- scale( f2$len.phon.ns, center=TRUE, scale=TRUE)
f2$LengthPhon.Head  = NULL
f2$LengthPhon.Prep  = NULL
f2$LengthPhon.Adj   = NULL
f2$LengthPhon.Noun  = NULL
f2$len.syll.ns      <- rowSums( f2[ , 9:12])  
f2$len.syll         <- scale( f2$len.syll.ns, center=TRUE, scale=TRUE)
f2$LengthSylHead    = NULL
f2$LengthSyll.Prep  = NULL
f2$LengthSyll.Adj   = NULL
f2$LengthSyll.Noun  = NULL
f2$assHL            <- rowSums( f2[ , 25:26])
f2$assHL.asin.ns    <- 2 * asin( sqrt( f2$assHL / 100))
f2$assHL.asin       <- scale( f2$assHL.asin.ns,  center = TRUE, scale = TRUE)
f2$AssPercent.HL.S  = NULL
f2$AssPercent.HL.P  = NULL
f2$LNSing.HS        = NULL
f2$LNSing.HP        = NULL   
f2$LNPlur.HS        = NULL   
f2$LNPlur.HP        = NULL   
f2$Freq1Mil.Head    = NULL
f2$FreqMil.Adj      = NULL
f2$FreqMil.Prep     = NULL
f2$FreqMil.N1       = NULL
f2$RelatedLH        = NULL
f2$lf.head          <- scale( f2$LogFr.Head,   center = TRUE, scale = TRUE)
f2$lf.prep          <- scale( f2$LogFreq.Prep, center = TRUE, scale = TRUE)
f2$lf.adj           <- scale( f2$LogFreq.Adj,  center = TRUE, scale = TRUE)
f2$lf.noun          <- scale( f2$LogFreq.N1,   center = TRUE, scale = TRUE)
f2$rel              <- scale( f2$RelatedHL,    center = TRUE, scale = TRUE)
f2$int              <- scale( f2$Integrated,   center = TRUE, scale = TRUE)
f2$plaus            <- scale( f2$Plausibility, center = TRUE, scale = TRUE)
f2$LogFr.Head       = NULL
f2$LogFreq.Prep     = NULL
f2$LogFreq.Adj      = NULL
f2$LogFreq.N1       = NULL
f2$RelatedHL        = NULL
f2$RelatedLH        = NULL
f2$Integrated       = NULL
f2$Plausibility     = NULL

f2                  <- f2[ , c(1:12,32:53)]
f2$len.char.ns      = NULL
f2$len.phon.ns      = NULL
f2$len.syll.ns      = NULL
f2$assHL.asin.ns    = NULL
f2$animacy          = NULL 
f2$all.asso         = NULL
f2$local.asso       = NULL
f2$assHL            = NULL 
f2$assHL           <- f2$assHL.asin
f2$assHL.asin       = NULL 
f2$res.int          <-scale( f2$res.int, center = TRUE, scale = TRUE)
f2$res.rel          <-scale( f2$res.rel, center = TRUE, scale = TRUE)
f2$res.asshl        <-scale( f2$res.asshl, center = TRUE, scale = TRUE)
f2$res.rel2         <-scale( f2$res.rel2, center = TRUE, scale = TRUE)


f2$corr             = f2$errcord - f2$errd 
f2$tot              = f2$errcord  
f2$elog             = log( ( f2$errd + .5) / ( f2$corr+.5) )  
f2$rates            = ifelse ( f2$errd == 0 & f2$corr == 0, 0, ( f2$errd / f2$tot) * 100) 
f2$v                = (1 / ( f2$errd + .5)) + (1 / ( f2$corr + .5)) 
f2$asin             = asin(sqrt ( f2$rates / 100))  
f2.all               <- f2

# -------------------- SEMREl 1 & PROP ITEMS: SET UP INPUT FILE(S) --------------------------------
f2                 <- read.csv("data/f2_critical_items.csv")
f2$X                = NULL
f2$X.1               = NULL
# -------------------- SEMREl 1 & PROP: PREPARE NUISANCE/PREDICTOR VARIABLES FOR ALL -------

f2                  <- subset( f2, subexpt != "Cat")
f2$item             <- as.factor( f2$item)
f2$item             <- droplevels( f2$item)

f2$len.char.ns      <- rowSums( f2[ ,  9:12])
f2$len.char         <- scale( f2$len.char.ns, center=TRUE, scale=TRUE)
f2$LengthChar.Head  = NULL
f2$LengthChar.Prep  = NULL
f2$LengthChar.Adj   = NULL
f2$LengthChar.Noun  = NULL
f2$len.phon.ns      <- rowSums( f2[ , 9:12])
f2$len.phon         <- scale( f2$len.phon.ns, center=TRUE, scale=TRUE)
f2$LengthPhon.Head  = NULL
f2$LengthPhon.Prep  = NULL
f2$LengthPhon.Adj   = NULL
f2$LengthPhon.Noun  = NULL
f2$len.syll.ns      <- rowSums( f2[ , 9:12])  
f2$len.syll         <- scale( f2$len.syll.ns, center=TRUE, scale=TRUE)
f2$LengthSylHead    = NULL
f2$LengthSyll.Prep  = NULL
f2$LengthSyll.Adj   = NULL
f2$LengthSyll.Noun  = NULL
f2$assHL            <- rowSums( f2[ , 25:26])
f2$assHL.asin.ns    <- 2 * asin( sqrt( f2$assHL / 100))
f2$assHL.asin       <- scale( f2$assHL.asin.ns,  center = TRUE, scale = TRUE)
f2$AssPercent.HL.S  = NULL
f2$AssPercent.HL.P  = NULL
f2$LNSing.HS        = NULL
f2$LNSing.HP        = NULL   
f2$LNPlur.HS        = NULL   
f2$LNPlur.HP        = NULL   
f2$Freq1Mil.Head    = NULL
f2$FreqMil.Adj      = NULL
f2$FreqMil.Prep     = NULL
f2$FreqMil.N1       = NULL
f2$RelatedLH        = NULL
f2$lf.head          <- scale( f2$LogFr.Head,   center = TRUE, scale = TRUE)
f2$lf.prep          <- scale( f2$LogFreq.Prep, center = TRUE, scale = TRUE)
f2$lf.adj           <- scale( f2$LogFreq.Adj,  center = TRUE, scale = TRUE)
f2$lf.noun          <- scale( f2$LogFreq.N1,   center = TRUE, scale = TRUE)
f2$rel              <- scale( f2$RelatedHL,    center = TRUE, scale = TRUE)
f2$int              <- scale( f2$Integrated,   center = TRUE, scale = TRUE)
f2$plaus            <- scale( f2$Plausibility, center = TRUE, scale = TRUE)
f2$LogFr.Head       = NULL
f2$LogFreq.Prep     = NULL
f2$LogFreq.Adj      = NULL
f2$LogFreq.N1       = NULL
f2$RelatedHL        = NULL
f2$RelatedLH        = NULL
f2$Integrated       = NULL
f2$Plausibility     = NULL

f2                  <- f2[ , c(1:12,32:53)]
f2$len.char.ns      = NULL
f2$len.phon.ns      = NULL
f2$len.syll.ns      = NULL
f2$assHL.asin.ns    = NULL
f2$animacy          = NULL 
f2$all.asso         = NULL
f2$local.asso       = NULL
f2$assHL            = NULL 
f2$assHL           <- f2$assHL.asin
f2$assHL.asin       = NULL   
f2$res.int          <-scale( f2$res.int, center = TRUE, scale = TRUE)
f2$res.rel          <-scale( f2$res.rel, center = TRUE, scale = TRUE)
f2$res.asshl        <-scale( f2$res.asshl, center = TRUE, scale = TRUE)
f2$res.rel2         <-scale( f2$res.rel2, center = TRUE, scale = TRUE)

f2$corr             = f2$errcord - f2$errd 
f2$tot              = f2$errcord  
f2$elog             = log( ( f2$errd + .5) / ( f2$corr+.5) )  
f2$rates            = ifelse ( f2$errd == 0 & f2$corr == 0, 0, ( f2$errd / f2$tot) * 100) 
f2$v                = (1 / ( f2$errd + .5)) + (1 / ( f2$corr + .5)) 
f2$asin             = asin(sqrt ( f2$rates / 100))  
f2.sr.prop               <- f2


