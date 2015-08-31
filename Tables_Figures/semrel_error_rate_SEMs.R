library(psych)
sem.f1errout <- read.table("data/SR_F1_errordata.txt", header = T)
sem.d <- sem.f1errout 
sem.d$pct <- ifelse(sem.d$errd == 0 & sem.d$errcord == 0, 0, (sem.d$errd / (sem.d$errcord)) * 100)


sem.relat.int.plur   <- subset(sem.d, related == "rel"   & semint  == "integ" & n2num == "plur")
sem.relat.int.sing   <- subset(sem.d, related == "rel"   & semint  == "integ" & n2num == "sing")
sem.relat.unint.plur <- subset(sem.d, related == "rel"   & semint  == "unint" & n2num == "plur")
sem.relat.unint.sing <- subset(sem.d, related == "rel"   & semint  == "unint" & n2num == "sing")
sem.unrel.int.plur   <- subset(sem.d, related == "unrel" & semint  == "integ" & n2num == "plur")
sem.unrel.int.sing   <- subset(sem.d, related == "unrel" & semint  == "integ" & n2num == "sing")
sem.unrel.unint.plur <- subset(sem.d, related == "unrel" & semint  == "unint" & n2num == "plur")
sem.unrel.unint.sing <- subset(sem.d, related == "unrel" & semint  == "unint" & n2num == "sing")


rip.1<-describe(sem.relat.unint.plur$pct)
ris.1<-describe(sem.relat.int.sing$pct)   
rup.1<-describe(sem.relat.unint.plur$pct)
rus.1<-describe(sem.relat.unint.sing$pct)
uip.1<-describe(sem.unrel.int.plur$pct)   
uis.1<-describe(sem.unrel.int.sing$pct)   
uup.1<-describe(sem.unrel.unint.plur$pct)
uus.1<-describe(sem.unrel.unint.sing$pct)

sem.f2errout <- read.table("data/SR_F2_errordata.txt", header = T)
sem.d <- sem.f2errout 
sem.d$pct <- ifelse(sem.d$errd == 0 & sem.d$errcord == 0, 0, (sem.d$errd / (sem.d$errcord)) * 100)


sem.relat.int.plur   <- subset(sem.d, related == "rel"   & integ  == "integ" & n2num == "plur")
sem.relat.int.sing   <- subset(sem.d, related == "rel"   & integ  == "integ" & n2num == "sing")
sem.relat.unint.plur <- subset(sem.d, related == "rel"   & integ  == "unint" & n2num == "plur")
sem.relat.unint.sing <- subset(sem.d, related == "rel"   & integ  == "unint" & n2num == "sing")
sem.unrel.int.plur   <- subset(sem.d, related == "unrel" & integ  == "integ" & n2num == "plur")
sem.unrel.int.sing   <- subset(sem.d, related == "unrel" & integ  == "integ" & n2num == "sing")
sem.unrel.unint.plur <- subset(sem.d, related == "unrel" & integ  == "unint" & n2num == "plur")
sem.unrel.unint.sing <- subset(sem.d, related == "unrel" & integ  == "unint" & n2num == "sing")


rip.2<-describe(sem.relat.unint.plur$pct)
ris.2<-describe(sem.relat.int.sing$pct)   
rup.2<-describe(sem.relat.unint.plur$pct)
rus.2<-describe(sem.relat.unint.sing$pct)
uip.2<-describe(sem.unrel.int.plur$pct)   
uis.2<-describe(sem.unrel.int.sing$pct)   
uup.2<-describe(sem.unrel.unint.plur$pct)
uus.2<-describe(sem.unrel.unint.sing$pct)

