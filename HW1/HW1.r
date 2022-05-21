#The code for Question 2
#question(b)
b <- rep(c("A","B"), each=10)
b
#question(c)
c <- rep(c("A","B"), times=10)
c
#question(d)
set.seed(9180)
d <-  sample(c("A","B"), 
                size=20, 
                replace=T, 
                prob=c(0.5,0.5))
d
#question(e)
set.seed(9180)
samp <- sample(c(1:20),size=10)
e <- rep(c("B"), times=20)
e[samp] <- "A"
e

#---------------------------------------------------------------

#The code for Question 3
#question(a)
data <- readRDS("D:/Plants.RDS")
a<-data[1:15,1]
mean(a)
median(a)
sd(a)
b<-data[16:30,1]
mean(b)
median(b)
sd(b)
#Defination of CDF
cdf<-function(y){
  ys<-seq(min(y)-sd(y)/length(y), max(y)+sd(y)/length(y),length=length(y)^3)
  ys<-sort(unique(c(y,ys)))
  py<-(table( c(y,ys) ) -1)/length(y)
  cbind(ys,cumsum(py))
}
#CDF image for the first group
plot( cdf(a) ,type="l",ylim=c(0,1),xlab="weight",ylab="F(weight)")
abline(h=c(0,1),col="gray")
#CDF image for the second group
plot( cdf(b) ,type="l",ylim=c(0,1),xlab="weight",ylab="F(weight)")
abline(h=c(0,1),col="gray")

#question(b)(i.)
g.ks<-function(yA,yB)
{ 
  sAB<-sort(unique(c(yA,yB)))
  FA<-cumsum( table( c(yA, sAB) ) -1 )/length(yA)
  FB<-cumsum( table( c(yB, sAB) ) -1 )/length(yB)
  max(abs(FA-FB))
}
g.ks(a,b)
g.vrs<-max(var(a)/var(b),var(b)/var(a))
g.vrs

#question(b)(ii.) and question (b)(iii.)
#Image and p-value for KS-statistic
g.ks<-function(yA,yB)
{ 
  sAB<-sort(unique(c(yA,yB)))
  FA<-cumsum( table( c(yA, sAB) ) -1 )/length(yA)
  FB<-cumsum( table( c(yB, sAB) ) -1 )/length(yB)
  max(abs(FA-FB))
}
y<- c(a,b)
x<-data[1:30,2]
set.seed(1)
g.null<-c()
for(nsim in 1:3000)
{
  xsim<-sample(x)
  g.null[nsim]<- g.ks(y[xsim==1],y[xsim==2])
}
hist(g.null,xlab=expression("KS-Statistic"), prob=T ,main="",col="lightblue")
abline(v=g.ks(a,b),col="red")
mean(g.null>=g.ks(a,b))

#Image and p-value for VRS
g.vrs<-max(var(a)/var(b),var(b)/var(a))
y<- c(a,b)
x<-data[1:30,2]
set.seed(1)
g.null<-c()
for(nsim in 1:10000)
{
  xsim<-sample(x)
  g.null[nsim]<- max(var(y[xsim==1])/var(y[xsim==2]),var(y[xsim==2])/var(y[xsim==1]))
}
hist(g.null,xlab=expression("Variance Ration Statistic"), prob=T ,main="",col="lightblue")
abline(v=g.vrs,col="red")
mean(g.null>=g.vrs)

#-------------------------------------------------------------

#The code for Question 4
#question(a)
g.obs <- 139/3
y<- c(256,159,149,54,123,248)
n.combs <- choose(6,3)
all.combinations <- combn(6,3)
g.null<- numeric(n.combs)
for ( i in 1:n.combs){
  A.indices <- all.combinations [ ,i]
  g.null[i] <- abs(mean(y[A.indices])-mean(y[-A.indices]))
}
mean(g.null>=g.obs)

#question(b)
p<- c(256,159,149)
s2A<-var(p)
q<- c(54,123,248)
s2B<-var(q)
s2p<-0.5*s2A+0.5*s2B
gt<-139/(sqrt(s2p)*sqrt(6))
gt
gt<-0.6994734
y<- c(256,159,149,54,123,248)
n.combs <- choose(6,3)
all.combinations <- combn(6,3)
g.null<- numeric(n.combs)
for ( i in 1:n.combs){
  A.indices <- all.combinations [ ,i]
  a<-y[A.indices]
  b<-y[-A.indices]
  s2A<-var(a)
  s2B<-var(b)
  s2p<-0.5*s2A+0.5*s2B
  g.null[i] <- abs(mean(a)-mean(b))/(sqrt(s2p)*(sqrt(2/3)))
}
mean(g.null>=gt)