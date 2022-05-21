#Question 1 -------------------------------------------
d.zinc <- readRDS("C:/Users/niuzc/Desktop/zinc.RDS")
d.zinc
F.crit<-qf(0.95,3,16)
F.crit

F.power<-1-pf(F.crit,3,16,ncp=0.4375)
F.power

n<-5
while(F.power<0.8){
  n<-n+1
  F.ncp<-n*(1^2+5^2+3^2+0^2)/400
  F.crit<-qf(0.95,3,4*n-4)
  F.power<-1-pf(F.crit,3,4*n-4,ncp=F.ncp)
}
n

F.crit<-qf(0.95,3,4*10-4)
F.ncp<-10*(1^2+5^2+3^2+0^2)/1
F.power<-1-pf(F.crit,3,4*10-4,ncp=F.ncp)
F.power

sigma<-1
while(F.power>=0.8){
  sigma<-sigma+1
  F.ncp<-10*(1^2+5^2+3^2+0^2)/(sigma^2)
  F.power<-1-pf(F.crit,3,4*10-4,ncp=F.ncp)
  F.power
}
sigma-1

#Question 2 ----------------------------------------
X<-matrix(c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0,0,-1,-1,0,0,0,0,
            1,1,-1,-1),nrow=8,ncol=4)
I4<-matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),nrow=4,ncol=4)
XTX.inv<-solve(t(X) %*% X, I4)
H<-X %*% XTX.inv %*% t(X)
H

#Question 3 -------------------------------------------
Y<-matrix(c(78.0,86.8,103.8,83.7,89.0,99.2,83.8,81.5,86.2))
X<-matrix(c(1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1),
          nrow=9,ncol=3)
I3<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
XTX.inv<-solve(t(X) %*% X,I3)
Beta.hat<-XTX.inv %*% t(X) %*% Y
Beta.hat

SSE<-t(Y-X %*% Beta.hat) %*% (Y-X %*% Beta.hat)
SSE
SE<-sqrt(SSE/18)
SE

t.crit<-qt(0.95,6)
interval.lower=Beta.hat[1]-SE*t.crit
interval.upper=Beta.hat[1]+SE*t.crit
c(interval.lower,interval.upper)
