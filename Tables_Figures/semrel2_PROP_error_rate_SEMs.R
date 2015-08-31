library(psych)
sem.f1errout <- read.table("data/SR2_F1_errprop.txt", header = T) # reads in all data from data file
sem.d <- sem.f1errout 
sem.d$pct <- ifelse(sem.d$errd == 0 & sem.d$errcord == 0, 0, (sem.d$errd / (sem.d$errcord)) * 100)

sem.relat.plur       <- subset(sem.d, related == "rel"   & n2num   == "plur")
sem.relat.sing       <- subset(sem.d, related == "rel"   & n2num   == "sing")
sem.assoc.plur       <- subset(sem.d, related == "assoc" & n2num   == "plur")
sem.assoc.sing       <- subset(sem.d, related == "assoc" & n2num   == "sing")
sem.unrel.plur       <- subset(sem.d, related == "unrel" & n2num   == "plur")
sem.unrel.sing       <- subset(sem.d, related == "unrel" & n2num   == "sing")

prop.rp.1       <-describe(sem.relat.plur$pct)        
prop.rs.1       <-describe(sem.relat.sing$pct)       
prop.ap.1       <-describe(sem.assoc.plur$pct)       
prop.as.1       <-describe(sem.assoc.sing$pct)       
prop.up.1       <-describe(sem.unrel.plur$pct)
prop.us.1       <-describe(sem.unrel.sing$pct)


sem.f2errout <- read.table("data/SR2_F2_errprop.txt", header = T) # reads in all data from data file
sem.d <- f2errout 
sem.d$pct <- ifelse(sem.d$errd == 0 & sem.d$errcord == 0, 0, (sem.d$errd / (sem.d$errcord)) * 100)

sem.relat.plur       <- subset(sem.d, related == "rel"   & n2num   == "plur")
sem.relat.sing       <- subset(sem.d, related == "rel"   & n2num   == "sing")
sem.assoc.plur       <- subset(sem.d, related == "assoc" & n2num   == "plur")
sem.assoc.sing       <- subset(sem.d, related == "assoc" & n2num   == "sing")
sem.unrel.plur       <- subset(sem.d, related == "unrel" & n2num   == "plur")
sem.unrel.sing       <- subset(sem.d, related == "unrel" & n2num   == "sing")

prop.rp.2       <-describe(sem.relat.plur$pct)        
prop.rs.2       <-describe(sem.relat.sing$pct)       
prop.ap.2       <-describe(sem.assoc.plur$pct)       
prop.as.2       <-describe(sem.assoc.sing$pct)       
prop.up.2       <-describe(sem.unrel.plur$pct)
prop.us.2       <-describe(sem.unrel.sing$pct)