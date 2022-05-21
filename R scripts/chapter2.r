###### Wheat CRD example


###### means, medians, std deviations and quantiles
yA<-c(11.4, 23.7, 17.9, 16.5, 21.1, 19.6)
yB<-c(26.9, 26.6, 25.3, 28.5, 14.2, 24.3)

nA<-length(yA)
nB<-length(yB)

mean(yA)
mean(yB)

median(yA)
median(yB)

sd(yA)
sd(yB)

quantile(yA,prob=c(.25,.75))
quantile(yB,prob=c(.25,.75))
######

######
cdf<-function(y){
   ys<-seq(min(y)-sd(y)/length(y), max(y)+sd(y)/length(y),length=length(y)^3)
   ys<-sort(unique(c(y,ys)))
   py<-(table( c(y,ys) ) -1)/length(y)
   cbind(ys,cumsum(py))
                }
######




###### plots of cdfs and distributions
pdf("fig2_1.pdf",family="Times",height=4,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
layout( matrix(c(1,2,3,1,2,3,4,5,7,4,6,7),nrow=4,ncol=3,byrow=T) )

#plot( sort(c(0,y)),(0:n)/n ,type="l",ylim=c(0,1),xlab="y",ylab="F(y)")
plot( cdf(y) ,type="l",ylim=c(0,1),xlab="y",ylab="F(y)")
 abline(h=c(0,1),col="gray")
hist(y,prob=T,main="")
plot(density(y,adj=2),main="",xlab="y")

#plot(sort(c(0,yA)),(0:nA)/nA,type="l",col="green",xlim=range(c(0,y)),
#          ylim=c(0,1),  xlab="y",ylab="F(y)")
plot(cdf(yA),type="l",col="green",xlim=range(y),
          ylim=c(0,1),  xlab="y",ylab="F(y)")
#lines(sort(c(0,yB)),(0:nB)/nB ,col="blue")
lines(cdf(yB),col="blue")
abline(h=c(0,1),col="gray")

hist(yA,prob=T,col="green",main="")
hist(yB,prob=T,col="blue",main="")

plot(density(yB,adj=2),col="blue",main="",xlab="")
lines(density(yA,adj=2),col="green")
dev.off()
######


###### randomization distribution
g.obs<-mean(yB)-mean(yA)

y<- c( 26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6)
x<-c("B", "A", "B", "A", "B", "B", "B", "A", "A", "A", "B", "A")

set.seed(1)
g.null<-c()
for(nsim in 1:10000)
{
  xsim<-sample(x)
  g.null[nsim]<- mean(y[xsim=="B"]) - mean(y[xsim=="A"] )
}


pdf("fig2_2.pdf",family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
par(mfrow=c(1,2))
hist(g.null,xlab=expression( bar(Y)[B] - bar(Y)[A]),prob=T ,main="",col="lightblue")
abline(v=g.obs,col="red")
hist(abs(g.null),xlab=expression(paste("|",bar(Y)[B]-bar(Y)[A],"|")),prob=T,main="",
   col="lightblue")
abline(v=abs(g.obs),col="red")
dev.off()

mean( abs(g.null)>=abs(g.obs))
######


##### other two-sample test statistics

g.median<-function(yA,yB){ abs(median(yA)-median(yB)) }

g.tstat<-function(yA,yB){ abs(t.test(yA,yB,var.equal=TRUE)$stat) }

g.ks<-function(yA,yB)
{ 
  sAB<-sort(unique(c(yA,yB)))
  FA<-cumsum( table( c(yA, sAB) ) -1 )/length(yA)
  FB<-cumsum( table( c(yB, sAB) ) -1 )/length(yB)
  max(abs(FA-FB))
}


###
muAv<-c(10,10)  ; muBv<-c(10,11) ; sAv<-c(1,2) ; sBv<-c(sqrt(5),2)

pn<-3
for(k in 1:2) {

set.seed(0)
nA<-40 ; nB<-40
x<-rep(c("A","B"),times=c(nA,nB))
muA<-muAv[k] ; muB<-muBv[k] ; sA<-sAv[k] ; sB<-sBv[k]
y<-rnorm(nA+nB,muA*(x=="A")+muB*(x=="B"), sA*(x=="A")+sB*(x=="B") )
###

yA<-y[x=="A"] ; yB<-y[x=="B"]
gobs<- c(g.tstat(yA,yB),g.ks(yA,yB))

Gsim<-NULL
for(s in 1:5000) {
  xsim<-sample(x)
  yAsim<-y[xsim=="A"] ; yBsim<-y[xsim=="B"]
  Gsim<-rbind(Gsim,
    c(g.tstat(yAsim,yBsim),g.ks(yAsim,yBsim)))
                   }

pdf( paste("fig2_",pn,".pdf",sep=""),family="Times",height=3,width=6)
par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
layout(matrix(c(1,3,2,3),byrow=TRUE,nrow=2,ncol=2) )
hist(y[x=="A"],xlim=range(y),col="lightgreen",main="",prob=TRUE,xlab=expression(y[A])) 
hist(y[x=="B"],xlim=range(y),col="lightblue",main="",prob=TRUE,xlab=expression(y[B]))

plot( cdf(yA),type="l" ,col="green",lwd=2,xlim=range(c(yA,yB)),xlab="y",
      ylab="F(y)")
lines( cdf(yB),type="l" ,col="blue",lwd=2)
dev.off()
pn<-pn+1

pdf( paste("fig2_",pn,".pdf",sep=""),family="Times",height=3,width=6)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,.75,0))

hist(Gsim[,1],col="gray",prob=TRUE,main="",xlab="t statistic")
abline(v=gobs[1],col="red")
hist(Gsim[,2],col="gray",prob=TRUE,main="",xlab="KS statistic")
abline(v=gobs[2],col="red")
cat(k,mean(Gsim[,1]>gobs[1]), mean(Gsim[,2]>gobs[2]) ,"\n")
dev.off()

pn<-pn+1
 }


