library(psych)
sem.f1errout <- read.table("data/SR2_F1_errcat.txt", header = T) # reads in all data from data file
sem.d <- sem.f1errout 
sem.d$pct <- ifelse(sem.d$errd == 0 & sem.d$errcord == 0, 0, (sem.d$errd / (sem.d$errcord)) * 100)


sem.relat.plur   <- subset(sem.d, related == "rel"   & n2num == "plur")
sem.relat.sing   <- subset(sem.d, related == "rel"   & n2num == "sing")
sem.unrel.plur   <- subset(sem.d, related == "unrel" & n2num == "plur")
sem.unrel.sing   <- subset(sem.d, related == "unrel" & n2num == "sing")

cat.rp.1 <-describe(sem.relat.plur$pct)  
cat.rs.1 <-describe(sem.relat.sing$pct) 
cat.up.1 <-describe(sem.unrel.plur$pct) 
cat.us.1 <-describe(sem.unrel.sing$pct) 


sem.f2errout <- read.table("data/SR2_F2_errcat.txt", header = T) # reads in all data from data file
sem.d <- sem.f2errout 
sem.d$pct <- ifelse(sem.d$errd == 0 & sem.d$errcord == 0, 0, (sem.d$errd / (sem.d$errcord)) * 100)

sem.relat.plur   <- subset(sem.d, related == "rel"   & n2num == "plur")
sem.relat.sing   <- subset(sem.d, related == "rel"   & n2num == "sing")
sem.unrel.plur   <- subset(sem.d, related == "unrel" & n2num == "plur")
sem.unrel.sing   <- subset(sem.d, related == "unrel" & n2num == "sing")

cat.rp.2 <-describe(sem.relat.plur$pct)  
cat.rs.2 <-describe(sem.relat.sing$pct) 
cat.up.2 <-describe(sem.unrel.plur$pct) 
cat.us.2 <-describe(sem.unrel.sing$pct)
