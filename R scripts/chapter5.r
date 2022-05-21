#####
set.seed(0)
m<-5
n<-4
x<-rep(1:m,rep(n,m))
mu<-rep(6,m)
y<-round(rnorm(m*n,mu[x],1),2)
y[1]<-y[1]-.07 ; y[2]<-y[2]+.07
y[14]<-y[14]+.05 ; y[15]<-y[15]-.05

ym<-tapply(y,x,mean)
t.test(y[x==which.max(ym)],y[x==which.min(ym)])
t.test(y[x==1],y[x==4])
summary(aov(y~as.factor(x)))
#####

ybar.t<-tapply(y,x,mean)
s2.t<-tapply(y,x,var)

SSE<- sum( (n-1)*s2.t )
SST<- n*sum( (ybar.t-mean(y))^2 )

MSE<-SSE/(m*(n-1))
MST<-SST/(m-1)

SSE

SST

MSE

MST



SSE<- (n-1)* sum( tapply(y,x,var) )
SST<- n*( sum( (tapply(y,x,mean) - mean(y))^2 )  )

MSE<-SSE/(m*(n-1))
MST<-SST/(m-1)



#####
pdf("fig5_1.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(y~x,col=x,pch=1,lwd=2,xlab="treatment",ylab="response time (seconds)" ,cex=1,xaxt="n")
axis(side=1,at=c(1:m),labels=c("A","B","C","D","E"))
abline(h=tapply(y,x,mean),col=1:m)
dev.off()
######



######
F.obs<-anova(lm(y~as.factor(x)))$F[1]

set.seed(1)
F.null<-NULL
for(nsim in 1:1000) 
{
  x.sim<-sample(x)
  F.null<-c(F.null, anova(lm(y~as.factor(x.sim)))$F[1] )
}

mean(F.null>=F.obs)

pdf("fig5_2.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
hist(F.null,xlim=range(c(F.null,F.obs)),col="lightblue",prob=T,nclass=15,ylab="",xlab="F",main="")
abline(v=F.obs,col="red")
dev.off()
######


######
pdf("fig5_3.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
coag<-read.table("coag.dat",header=T)
attach(coag)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(as.integer(diet),jitter(ctime),xlab="diet",xaxt="n",
     ylab="coagulation time",pch=as.character(diet),col=as.integer(diet))
tmeans<-tapply(ctime,diet,mean)
#abline(h= tmeans, col=c(1:4))
points(1:4,tmeans,col=1:4,pch=16)
dev.off()
######


######
pdf("fig5_4.pdf",family="Times",height=6,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

par(mfrow=c(2,1),mar=c(3,3,1,1),mgp=c(1.75,.75,0))
xf<-seq(0.1,20,length=1000)
plot(xf, df(xf,3,20),type="l",col="red",xlab="F",ylab="density")
lines(xf, df(xf,3,10),col="orange")
lines(xf, df(xf,3,5),col="yellow")
lines(xf, df(xf,3,2),col="green")
abline( v=qf(.95,3,c(20,10,5,2)),col=c("red","orange","yellow","green"))
abline(h=0,col="gray")
legend(10,.6,c("F(3,20)","F(3,10)","F(3,5)","F(3,2)"),lwd=rep(1,4),
       col=c("red","orange","yellow","green"),bty="n")

xf<-seq(0.1,20,length=1000)
plot(xf, pf(xf,3,20),type="l",col="red",xlab="F",ylab="CDF")
lines(xf, pf(xf,3,10),col="orange")
lines(xf, pf(xf,3,5),col="yellow")
lines(xf, pf(xf,3,2),col="green")
abline( v=qf(.95,3,c(20,10,5,2)),col=c("red","orange","yellow","green"))
abline(h=0,col="gray")
legend(10,.6,c("F(3,20)","F(3,10)","F(3,5)","F(3,2)"),lwd=rep(1,4),
       col=c("red","orange","yellow","green"),bty="n")
abline(h=.95,col="gray",lty=2)
dev.off()
######



######
set.seed(1)
###randomization test
Fobs<-anova(lm(ctime~diet))$F[1]
Fsim<-NULL
for(nsim in 1:1000) {
diet.sim<-sample(diet) 
Fsim<-c(Fsim, anova(lm(ctime~diet.sim))$F[1] )
cat(nsim,"\n")
                     }

mean(Fsim>Fobs)
1-pf(Fobs,3,20)

pdf("fig5_5.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))
f<-seq(.005,14,length=500)
hist(Fsim,prob=T,ylim=c(0,.75),xlab="F",ylab=expression(italic(p(F[{list(3,20)}]))),col="lightblue",main="",
   nclass=15)
lines(f,df( f,3,20),lwd=2)
abline(v=c( qf(.95,3,20), quantile(Fsim,.95)),col=c("black","lightblue" ),
    lwd=c(1,2) )
legend(3.25,.5,legend=c("randomization 95th percentile","normal theory 95th percentile"),lwd=c(2,1),col=c("lightblue","black"),bty="n")
dev.off()
######

###
tapply( coag[,2],coag[,1],function(y){ t.test(y)$conf.int[1:2] } )



###



######
pdf("fig5_6.pdf",family="Times",height=2,width=6)
par(mar=c(3,3,1.5,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,3))
m<-4
alpha<-0.05
n<-seq(2,10)
F.crit<- qf(1-alpha,m-1,m*(n-1))
var.ratio<-1   #var.between/var.within
plot(n,F.crit,type="h",lwd=4,xlab=expression(italic(n)),col="gray")
lambda<-  m*n* var.ratio
plot(n,lambda,type="h",lwd=4,xlab=expression(italic(n)),ylab=expression(lambda),col="gray")
plot(n, 1-pf( F.crit,m-1,m*(n-1),ncp=lambda) ,ylab="power",type="h",lwd=4,xlab=expression(italic(n)),col="gray")
mtext(expression(paste(m==4,"   ",alpha==0.05,"   ","var.b/var.w=1")),side=3,outer=T,line=-1.15,cex=.8)
dev.off()
######

######
pdf("fig5_7.pdf",family="Times",height=2,width=6)
par(mar=c(3,3,1.5,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,3))

m<-4
alpha<-0.05
n<-seq(2,10)
F.crit<- qf(1-alpha,m-1,m*(n-1))
var.ratio<-2   #var.between/var.within
plot(n,F.crit,type="h",lwd=4,xlab=expression(italic(n)),col="gray")
lambda<-  m*n* var.ratio
plot(n,lambda,type="h",lwd=4,xlab=expression(italic(n)),ylab=expression(lambda),col="gray")
plot(n, 1-pf( F.crit,m-1,m*(n-1),ncp=lambda) ,ylab="power",type="h",lwd=4,xlab=expression(italic(n)),col="gray")
mtext(expression(paste(m==4,"   ",alpha==0.05,"   ","var.b/var.w=2")),side=3,outer=T,line=-1.15,cex=.8)
dev.off()
######



######
pdf("fig5_8.pdf",family="Times",height=6,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(3,2))
n<-c(20,50,100)
for(i in 1:3) {
set.seed(1)
y<-rnorm(n[i],0,1) 
hist(y, prob=T,xlim=c(-max(abs(y)), max(abs(y))) ,nclass=12,main="",col="lightblue",ylim=c(0,.55))
x<-seq(-3,3,length=100)
lines(x,dnorm(x,0,sd(y)),type="l")
qqnorm(y,main="") ;qqline(y)
                 }
dev.off()
######


#####
crab<-read.table("crab.all.dat")

samp_mean<-tapply(crab[,2],crab[,1],mean)
samp_sd<-tapply(crab[,2],crab[,1],sd)
samp_med<-tapply(crab[,2],crab[,1],median)

cbind(samp_mean,samp_med,samp_sd)

anova(lm(crab[,2]~as.factor(crab[,1])))

pdf("fig5_9.pdf",family="Times",height=6,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(3,2))
for(i in 1:6){hist(crab[crab[,1]==i,2],main="",col="light blue",prob=T,
              xlim=range(crab[,2]),nclass=12,xlab="population")
              lines(density(crab[crab[,1]==i,2],adj=2))
              mtext(paste("site",i,sep=""),line=-.2,cex=.7) }
dev.off()
######


#####
pdf("fig5_10.pdf",family="Times",height=2.75,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
fit1<-lm(crab[,2]~as.factor(crab[,1]))
hist(fit1$res,main="",prob=T,col="light blue",xlab="residuals",ylab="") ; lines(density(fit1$res,adj=2))
qqnorm(fit1$res,main=""); qqline(fit1$res)
dev.off()
#####


#####
pdf("fig5_11.pdf",family="Times",height=2.75,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))
plot(fit1$res~jitter(fit1$fit),xlab="fitted value",ylab="residual",cex=.6,
      col="blue")
abline(h=0,col="gray")
dev.off()
#####


######
pdf("fig5_12.pdf",family="Times",height=4,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,1))
plot(crab[,2]~as.factor(crab[,1]),col="light blue",ylab="crab population",xlab="site")
plot(log(crab[,2]+1/6) ~as.factor(crab[,1]),col="light blue",ylab="log crab population",xlab="site")
dev.off()
######


#####
crabt<-round( cbind(
tapply(log(crab[,2]+1/6),crab[,1],mean),
tapply(log(crab[,2]+1/6),crab[,1],sd) ),2)

crabt[order(crabt[,1]),]

anova(lm(log(crab[,2]+1/6)~as.factor(crab[,1])))
fit2<-lm(log(crab[,2]+1/6)~as.factor(crab[,1]))

pdf("fig5_13.pdf",family="Times",height=2.5,width=6.5)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,3))
hist(fit2$res,col="lightblue",main="",xlab="residual",ylab="",prob=TRUE)
qqnorm(fit2$res,main="") ; qqline(fit2$res)
plot(fit2$res~fit2$fit,xlab="fitted value",ylab="residual")
abline(h=0,col="gray")
dev.off()
######


#####
crab<-read.table("crab.all.dat")
samp_mean<-tapply(crab[,2],crab[,1],mean)
samp_sd<-tapply(crab[,2],crab[,1],sd)
tmp<-round( cbind( samp_sd,samp_mean,log(samp_sd),log(samp_mean)) ,2)
tmp[order(tmp[,1]),]
log_sd<-tmp[,3]
log_mean<-tmp[,4]

pdf("fig5_14.pdf",family="Times",height=4.5,width=4.5)
par(mfrow=c(1,1))
plot(log_sd~log_mean,xlab="log(mean)",ylab="log(sd)")
abline(lm(log_sd~log_mean))
lm(log_sd~log_mean)
dev.off()


#####



#####
y1<-c(12.2,11.4,12.4)
y2<-c(16.0,15.5,16.5)
y3<-c(18.6,20.2,18.2)
y4<-c(17.6,19.3,17.1)
y5<-c(18.0,16.4,16.6)

y<-c(y1,y2,y3,y4,y5)
x<-rep(10*(1:5),rep(3,5))

tapply(y,x,mean)

pdf("fig5_15.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,1))
plot(y~x,xlab="plant density",ylab="grain yield",col=x/10,pch=16)
dev.off()

anova(lm(y~as.factor(x)))






