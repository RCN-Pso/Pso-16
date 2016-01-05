mydata = read.csv("C:/WD/Possner/Possner/Possner1.csv", sep = ";" ,dec = ",")  # read csv file 


library(Rcmdr)
library(ggplot2)
library(plyr)


mydata<- na.omit(mydata)
T123=(mydata$TBase1+mydata$TBase2+mydata$TBase3)/3
mydata$T123=T123
attach(mydata)

t.test(mydata$TBase1,mydata$TBase2 ,alternative ="less", paired = TRUE, var.equal = FALSE, conf.level = 0.95)

groups = rep(letters[1:4], each = 15)
groups2=rep(letters[2], each = 15)

groups=c(groups,groups2)
v=as.vector(as.matrix(mydata[,1:4]))
v1=as.vector(as.matrix(mydata[,4]))
v=c(v,v1)

bartlett.test(v, groups)
fit = lm(formula = v ~ groups)
anova (fit)


fit1=aov(v ~ groups )


pairwise.t.test(v, groups,p.adjust.method="holm", paired=T)

summary(fit1) # display Type I ANOVA table
drop1(fit1,~.,test="F") # type III SS and F Tests
TukeyHSD(fit1)
pairwise.t.test(v,groups, p.adjust.method ="holm",paired = TRUE,alternative ="less")

###############################################################################################
mydata = read.csv("C:/WD/Possner/Possner/Possner1.csv", sep = ";" ,dec = ",")  # read csv file 

v=as.vector(as.matrix(mydata[,1:4]))
g=rep(1:4, each = 15)



Change<-factor(rep(c("Baseline","Evaluation","Baseline","Evaluation"),each=15))
Amount<-factor(rep(c("Left","Right"),each=30))
plotMeans(v11, Change, Amount, error.bars="se",lty=c(1,1),pch=20,lwd=c(2,2),main = "Posner reaktion time",ylab = "Reaction time (s)",xlab = "",col=c("firebrick1","dodgerblue3"),legend.lab="")	


Change2<-factor(rep(c("Baseline","Evaluation"),each=15))
Amount2<-factor(rep(c(""),each=30))

v13=c((mydata$Base1ToT+mydata$Base2ToT+mydata$Base2ToT)/3,mydata$EvalToT)
plotMeans(v13, Change2, Amount2, error.bars="se",lty=c(1,1),pch=20,lwd=c(2,2),main = "Posner reaktion time",ylab = "Reaction time (s)",xlab = "",col=c("firebrick1","dodgerblue3"),legend.lab="")	

Change1<-factor(rep(c("Baseline","Evaluation"),each=15))
Amount1<-factor(rep(c(""),each=30))
v12=c(mydata$PreTraining,mydata$Eval)
plotMeans(v12, Change1, Amount1, error.bars="se",lty=c(1,1),pch=20,lwd=c(2,2),main = "Posner missed targets",ylab = "Percent misses (%)",xlab = "",col=c("dodgerblue2","firebrick1"),legend.lab="")	









#####################################################
SKIT!


##########

str(A$v11)

length(v11)

A=as.data.frame(cbind(v11,as.character(Amount),as.character(Change)))
A$v11=v11*1000

tgc <- summarySE(A, measurevar="v11", groupvars=c("V3","V2"))

ggplot(tgc, aes(x=V3, y=v11, colour=V2)) + 
  geom_errorbar(aes(ymin=v11-se, ymax=v11+se), width=.1) +
  geom_line() +
  geom_point()



# Use a consistent y range
ymax <- max(A$v11)
ymin <- min(A$v11)



ggplot(tgc, aes(x=V3, y=v11, group=2)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=v11-ci, ymax=v11+ci), colour="red") +
  geom_errorbar(width=.1, aes(ymin=v11-ci, ymax=v11+ci), data=tgc) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(ymin,ymax)





summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}













kruskal.test(v~g)


g=rep(1:4, each = 15)
groups = rep(letters[1:4], each = 15)

fit = lm(formula = v ~ groups)
anova (fit)
fit1=aov(v ~ groups )
summary(fit1)
kruskal.test(v~g)
TukeyHSD(fit1)
pairwise.t.test(v,groups, p.adjust.method ="holm",paired = TRUE,alternative ="less")


p=c(0.008,0.008,0.034)
p.adjust(p, method = "holm", n = length(p))



