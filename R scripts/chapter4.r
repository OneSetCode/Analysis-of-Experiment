######
y<- c( 26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6)
x<-c("B", "A", "B", "A", "B", "B", "B", "A", "A", "A", "B", "A")

yA<-y[x=="A"] ; yB<-y[x=="B"] ; nA<-sum(x=="A") ; nB<-sum(x=="B")

sp2<- (  (nA-1)*var(yA) + (nB-1)*var(yB) )/(nA-1+nB-1)
sp<-sqrt(sp2)

alpha<-.05
tc<-qt(1-alpha/2,nA+nB-2)

mean(yB)-mean(yA) + c(-1,1)*sp*sqrt(1/nA+1/nB)*tc



######



######
pdf("fig4_1.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
nA<-nB<-6
x<-seq(-3,7,length=100)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(c(-3,7),c(0,.4),type="n",xlab=expression(italic(t)),ylab="")
for(ncp in c(0,1,2) ) {
lines(x,dt(x,nA+nB-2, ncp),type="l",col=3+ncp,lwd=2)
                             }
abline(v=0,lty=2,col="gray")
legend(4,.36,
#  legend=c("gamma=0","gamma=1","gamma=2"), 
  legend=c(expression(gamma==0),expression(gamma==1),expression(gamma==2)),
  lwd=c(2,2,2),col=c(3,4,5),bty="n")
dev.off()
######


######
sigma<-sqrt(31.07)
del<-2.4
nA<-6
nB<-6

x<-seq(-5,5,length=100)
plot(x,dt(x,nA+nB-2, del/(sigma*sqrt(1/nA+1/nB))),type="l")

sigma<-sqrt(31.07)
delta<-2.4
nA<-6
nB<-6
alpha<-.05
t.gamma<- delta/(sigma*sqrt(1/nA +1/nB))

t.rcrit<-qt(1-alpha/2,nA+nB-2)

t.power<-pt(-t.rcrit,nA+nB-2, ncp=t.gamma) +
         1-pt(t.rcrit,nA+nB-2, ncp=t.gamma)
######


######
pdf("fig4_2.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
plot(c(-6,6),c(0,.4),type="n",xlab=expression(italic(t)),ylab="")
x<-seq(-6,6,length=100)
qc<-  qt(.975,10)
for(ncp in c(0,1) ) {
lines(x,dt(x,nA+nB-2, ncp),type="l",col=3+ncp,lwd=2)
                             }
abline(v=0,lty=2,col="gray")
abline(v=qt(c(.025,.975),10))
legend(2.5,.36,legend=c(expression(gamma==0),expression(gamma==1)), lwd=c(1,1,1),col=c(3,4,5),bty="n")

xp<-c( qc,qc,x[x>qc],max(x))
yp<-c(0,dt( c(qc,x[x>qc]),nA+nB-2,ncp=1),0)
polygon( xp, yp,col="lightblue" ,border="blue" )

yp<-c(0,dt( c(qc,x[x>qc]),nA+nB-2,ncp=0),0)
polygon( xp, yp,col="lightgreen" ,border="green" )

xp<-c( min(x),x[x< -qc],-qc,-qc)
yp<-c(0,dt( c(x[x< -qc],-qc),nA+nB-2,ncp=0),0)
polygon( xp, yp,col="lightgreen" ,border="green" )

yp<-c(0,dt( c(x[x< -qc],-qc),nA+nB-2,ncp=1),0)
polygon( xp, yp,col="lightblue",border="blue"  )
abline(h=0,col="gray")

dev.off()
######



######
delta<-5 ; s2<- (  (nA-1)*var(yA) + (nB-1)*var(yB) )/(nA-1+nB-1)

alpha<-0.05 ; n<-seq(6,30)

t.crit<- qt(1-alpha/2,2*n-2)

t.gamma<-delta/sqrt(s2*(1/n+1/n))

t.power<-1-pt( t.crit,2*n-2,ncp=t.gamma)+
           pt(-t.crit,2*n-2,ncp=t.gamma)

t.normal.power<- 1- pnorm( t.crit, mean=t.gamma ) +
                    pnorm(-t.crit, mean=t.gamma )

######

pdf("fig4_3.pdf",family="Times",height=2.75,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
plot(n,t.gamma,xlab=expression(italic(n)),ylab=expression(gamma))
plot(n, t.power,ylim=range(c(t.power,t.normal.power)),xlab=expression(italic(n)), ylab="power") 
points(n,t.normal.power,pch=2)
legend(15,.65,legend=c("power","normal approx"),pch=c(1,2),cex=.8)
dev.off()


#####
pdf("fig4_4.pdf",family="Times",height=3,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

n<-10
t.crit<- qt(1-alpha/2,2*n-2)

par(mfrow=c(1,2))
cols<-c("blue","green","red")
del<-seq(-4,4,length=100)
theta.123<- cbind( del/sqrt(1),del/sqrt(2),del/sqrt(3))
plot(c(-4,4),c(0,1),type="n",xlab=expression(mu[B]-mu[A]), ylab="power")
for(j in 1:3) {
  t.power<-1-pt( t.crit,2*n-2,ncp=theta.123[,j]/sqrt(2/n))+
           pt(-t.crit,2*n-2,ncp= theta.123[,j]/sqrt(2/n))
  lines(del,t.power,type="l",col=cols[j],lwd=2) 
              } 
legend(-4.5,.4,
   legend=c(expression(sigma==1),expression(sigma==2),expression(sigma==3)), 
   bty="n",lwd=c(1,1,1),col=c("blue","green","red"))

theta<-seq(-3,3,length=100)
t.gamma<- theta/sqrt(1/n+1/n)
t.power<- 1-pt( t.crit,2*n-2,ncp=t.gamma)+
           pt(-t.crit,2*n-2,ncp=t.gamma)
plot(theta, t.power,xlab=expression(paste("(",mu[B]-mu[A],")/",sigma)),
 ylab="power",type="l")
dev.off()

#####

pdf("fig4_4a.pdf",family="Times",height=3,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
alpha<-.05
n<-10
t.crit<- qt(1-alpha/2,2*n-2)

par(mfrow=c(1,1))
cols<-c("blue","green","red")
del<-seq(-4,4,length=100)
theta.123<- cbind( del/sqrt(1),del/sqrt(2),del/sqrt(3))
plot(c(-4,4),c(0,1),type="n",xlab=expression(mu[B]-mu[A]), ylab="power")
for(j in 1:3) {
  t.power<-1-pt( t.crit,2*n-2,ncp=theta.123[,j]/sqrt(2/n))+
           pt(-t.crit,2*n-2,ncp= theta.123[,j]/sqrt(2/n))
  lines(del,t.power,type="l",col=cols[j],lwd=2)
              }
legend(-3.5,.4,
   legend=c(expression(sigma==1),expression(sigma==2),expression(sigma==3)),
   bty="n",lwd=c(1,1,1),col=c("blue","green","red"))
dev.off()

pdf("fig4_4b.pdf",family="Times",height=3,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))

theta<-seq(-3,3,length=100)
t.gamma<- theta/sqrt(1/n+1/n)
t.power<- 1-pt( t.crit,2*n-2,ncp=t.gamma)+
           pt(-t.crit,2*n-2,ncp=t.gamma)
plot(theta, t.power,xlab=expression(paste("(",mu[B]-mu[A],")/",sigma)),
 ylab="power",type="l")
dev.off()


######






######
pdf("fig4_3.pdf",family="Times",height=6,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(2,1))
delta<-3
sigma<-6
alpha<-.05
x<-seq(-7,7,length=100)
plot(range(x),c(0,.45),type="n",xlab="t",ylab="p(t)")
colr<-NULL
for(n in c(5,20,80)) {

ncp<-delta/(sigma*sqrt(1/n+1/n)) 

lines(x,dt(x,df=n+n-2),lwd=2)
#abline(v=qt(1-alpha/2,n+n-2),col="red")
abline(v=c(-2,0,2),col=c("red","gray","red"))
colr<-c(colr, rgb(ncp/3.17,.5,1-ncp/3.17))
lines(x,dt(x,ncp=ncp,df=n+n-2),lwd=1,col=rgb(ncp/3.17,.5,1-ncp/3.17),lty=1)
                             }
legend( -7,.4,col=colr,legend=c(5,20,80),lty=c(1,1,1),bty="n",lwd=c(2,2,2))
t.pow<-NULL
N<- seq(5,100,by=5)
for(n in N) {
ncp<-delta/(sigma*sqrt(1/n+1/n)) 
t.rcrit<-qt(1-alpha/2,n+n-2)
t.pow<-c(t.pow,pt(-t.rcrit,n+n-2, ncp=ncp) +
               1-pt(t.rcrit,n+n-2, ncp=ncp) )

}
plot(N,t.pow,type="l",xlab=expression(paste(n[A],"=",n[B])),ylab="power")
dev.off()
#######










