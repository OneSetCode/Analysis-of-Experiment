#Question 1 -------------------------------------------------------------
dat <- data.frame(responses = c(2600, 2900, 2000, 2200,3200,
                                35000,23000,20000,30000,27000,
                                2900000,2300000,1700000,2900000,2000000),
                  treatment = as.factor(c(rep("1",5),rep("2",5),rep("3",5))))

fit.count<-lm(dat$responses~dat$treatment)
anova(fit.count)
fit.count$residuals

hist(fit.count$residuals,prob=T)
plot(fit.count)

vars=tapply(dat$responses,as.factor(dat$treatment),var)
vars

sd=tapply(dat$responses,as.factor(dat$treatment),sd)
means=tapply(dat$responses,as.factor(dat$treatment),mean)
plot(log(means),log(sd))
lm(log(sd)~log(means))


fit.log<-lm(log(dat$responses)~dat$treatment)
anova(fit.log)
fit.log$residuals
hist(fit.log$residuals,prob=T)
plot(fit.log)

vars=tapply(log(dat$responses),dat$treatment,var)
vars


#Question 2----------------------------------------------------------
d.len<-read.table('C:/Users/niuzc/Desktop/lentil.dat',header = TRUE)
stripchart(d.len$TR~d.len$Y,ylab='d.len$Y')
d.len$TR<-factor(d.len$TR)

mod<-lm(Y~TR,data=d.len)
anova(mod)

hist(mod$residuals)
plot(mod)

vars=tapply(d.len$Y,as.factor(d.len$TR),var)  
vars 

mat.contr<-matrix(c(-6,1,1,1,1,1,1,0,-1,-1,-1,1,1,1,0,
                   2,-1,-1,2,-1,-1,0,0,-1,1,0,-1,1,0,
                   -2,1,1,2,-1,-1,0,0,1,-1,0,-1,1),nrow=6,byrow=TRUE)
library(multcomp)
fit.mc <- glht(mod, linfct = mcp(TR=mat.contr))
summary(fit.mc, test = adjusted("none"))


#Question 3-------------------------------------------------------
y <- c(9, 12, 10, 8, 15,
       20, 21, 23, 17, 30,
       6, 5, 8, 16, 7)
type <- c(rep('1',5),rep('2',5),rep('3',5))
circ <- data.frame(Type = type, Y = y)
circ$Type <- as.factor(circ$Type)

mod.circ<-lm(Y~Type,data=circ)
anova(mod.circ)

hist(mod.circ$residuals,prob=T)
plot(mod.circ)

vars=tapply(circ$Y,circ$Type,var)
vars

library(multcomp)
mat.cont <- matrix(c(1,-2,1,1,0,-1),nrow=2,byrow=TRUE)
circ.mc <- glht(mod.circ, linfct = mcp(Type = mat.cont))
summary(circ.mc, test = adjusted("holm"))

