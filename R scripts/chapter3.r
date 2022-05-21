###### Wheat CRD example
y<- c( 26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6)
x<-c("B", "A", "B", "A", "B", "B", "B", "A", "A", "A", "B", "A")


n<-length(y)
tapply(y,x,mean)

n<-length(y)

yA<-y[x=="A"]
yB<-y[x=="B"]

nA<-length(yA)
nB<-length(yB)
######




######
pdf("fig3_1.pdf",family="Times",height=6,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,2))

ysup<-seq(5,35,length=50)
plot(ysup,dnorm(ysup,mean(yA),sd(yA)),type="l",col="green",bty="n",xlab="",
       ylab="",xaxt="n")
abline(h=0)
abline(v=mean(yA),lty=2)
text( mean(yA)-1.5,.005,expression(mu[A]))
mtext(" `All possible' A wheat yields",side=3 )
arrows(28,.05,37,.05,xpd=T)
text( 30, .06, "random sampling",xpd=T)

hist(yA,col="green",xlim=c(5,35),prob=T,main="",ylab="")
mtext(" Experimental samples",side=3 )
text(9,.06  , expression( paste( bar(y)[A],"=18.37" )))
text(9,.05  , expression( paste( s[A],"=4.23" )))


plot(ysup,dnorm(ysup,mean(yB),sd(yB)),type="l",col="blue",bty="n",xlab="",
       ylab="",xaxt="n")
abline(h=0)
abline(v=mean(yB),lty=2)
text( mean(yB)-1.5,.005,expression(mu[B]))
mtext(" `All possible'  B wheat yields",side=3 )
arrows(28,.05,37,.05,xpd=T)
text( 30, .06, "random sampling",xpd=T)

hist(yB,col="blue",xlim=c(5,35),prob=T,main="",ylab="")
mtext(" Experimental samples",side=3 )
text(9,.06  , expression( paste( bar(y)[B],"=24.30" )))
text(9,.05  , expression( paste( s[B],"=5.15" )))
dev.off()
######

###### CLT example
n<-5 ; y<-rexp(n) ; hist(y)

ybar<-NULL
for(s in 1:1000) 
{
  y<-rexp(n)
  ybar<-c(ybar,mean(y))
  hist(ybar,prob=TRUE)
}
######



######
pdf("fig3_2.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
x<-seq(0,30,length=100)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(x,dchisq(x,9),type="l",col="blue",xlab=expression(italic(X)),ylab=expression(italic(p(X))),lwd=2)
lines(x,dchisq(x,10),type="l",col="green",lwd=2)
lines(x,dchisq(x,11),type="l",col="red",lwd=2)
legend(20,.08,legend=c("n=9","n=10","n=11"),
              lty=c(1,1,1), col=c("blue","green","red"),bty="n")
dev.off()
######


######
pdf("fig3_3.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
x<-seq(-3,3,length=100)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(x,dnorm(x),type="l",col="gray",xlab=expression(italic(t)),ylab=expression(italic(p(t))),lwd=2)
lines(x,dt(x,3),type="l",col="blue",lwd=2)
lines(x,dt(x,6),type="l",col="green",lwd=2)
lines(x,dt(x,12),type="l",col="red",lwd=2)
legend(1.5,.3,legend=c("n=3","n=6","n=12",
   expression(paste("n=",infinity,sep=""))),
              lty=c(1,1,1), col=c("blue","green","red","gray"),bty="n")
dev.off()
######


######
#pdf("fig3_4.pdf",family="Times",height=3,width=6)
#par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
#n<-8
#x<-seq(-3,3,length=100)
#plot(x,dt(x,n-1),type="l",xlab="t",ylab="density",lwd=2,col="gray")
#abline(v=0,lty=2)
#alpha=.05
#abline(v=c( qt(1-alpha/2,n-1), qt(alpha/2,n-1) ) )
#dev.off()
######


######
y<- c( 26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6)
x<-c("B", "A", "B", "A", "B", "B", "B", "A", "A", "A", "B", "A")
t.test(y[x=="B"],y[x=="A"],var.equal=TRUE)

t.stat.obs<-t.test(y[x=="B"],y[x=="A"],var.equal=TRUE)$stat

pdf("fig3_4.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
tseq<-seq(-5,5,length=100)
plot( tseq,dt(tseq,10),type="l",xlab=expression(italic(T)), ylab=expression(italic(p(T))) )
abline(v= qt(c(0.025,.5,.975),10), col=c("red","gray","red"),lwd=(c(2,1,2)))
abline(h=0,col="gray")
abline(v=t.stat.obs,col="green",lwd=2)
dev.off()
######


###### randomization distribution of the t-statistic
set.seed(1)
t.stat.obs<-t.test( y[x=="A"],y[x=="B"], var.equal=T)$stat
t.stat.sim<-real()
for(s in 1:10000)
{
  xsim<-sample(x)  
  tmp<-t.test(y[xsim=="B"],y[xsim=="A"],var.equal=T)
  t.stat.sim[s]<-tmp$stat
}

mean( abs(t.stat.sim) >= abs(t.stat.obs) )


pdf("fig3_5.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
hist(t.stat.sim,xlab=expression(italic(paste("t(",Y[A],",",Y[B],")",sep="")))
      ,prob=T ,main="",ylim=range(dt(tseq,10)) ,breaks=20,col="light blue")
lines(tseq,dt(tseq,10),lwd=3)
abline(v=c(-1,1)*t.stat.obs,col="green")
dev.off()
######



######
pdf("fig3_6.pdf",family="Times",height=6,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(4,4))

y<- c( 26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6)
x<-c("B", "A", "B", "A", "B", "B", "B", "A", "A", "A", "B", "A")



qqnorm(y[x=="A"],main="",col="green",pch=16) ; qqline(y[x=="A"])
qqnorm(y[x=="B"],main="",col="blue",pch=16) ; qqline(y[x=="B"])

set.seed(1)
for(i in 1:14) {
ysim<-rnorm(6) 
qqnorm(ysim,main="",pch=16) ; qqline(ysim) 
               }
dev.off()
########









