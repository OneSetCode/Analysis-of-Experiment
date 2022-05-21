#Question 1-------------------------------------------
set.seed(123)
A<-rnorm(30,mean=0,sd=1)
B<-rnorm(10,mean=0,sd=2)
nA<-30
nB<-10
Am<-mean(A)
Bm<-mean(B)
s2A<-var(A)
s2B<-var(B)
s2p <- ((nA-1)/((nA-1)+(nB-1)))*s2A+((nB-1)/((nA-1)+(nB-1)))*s2B
sp<-sqrt(s2p)
tAB<-(Bm-Am)/(sp*sqrt((1/nA)+(1/nB)))
tAB


set.seed(123)
t.obs<-tAB
X=c(A,B)
t.null<-NULL
for(s in 1:10000){
  xsim <- sample(c(rep('A',30),rep('B',10)))
  t.null[s] <- abs(mean(X[xsim=="B"]) - mean(X[xsim=="A"]))/
    (sqrt(29*var(X[xsim=="A"])/38+9*var(X[xsim=="B"])/38)*sqrt(2/15))
}
mean(t.null>=t.obs)


cv<-qt(0.975,38)
cv


t.pvalue<-t.test(A,B,var.equal=TRUE)$p.value
t.pvalue


#Question 2--------------------------------------------------------------
set.seed(123)
twAB<-(mean(B)-mean(A))/sqrt(var(A)/nA+var(B)/nB)
twAB

s2A<-var(A)
s2B<-var(B)
Vm<-(s2A/nA+s2B/nB)^2/((s2A/nA)^2/(nA-1)+(s2B/nB)^2/(nB-1))
Vm

wcv<-qt(0.975,14.56476)
wcv

tw.pvalue<-t.test(A,B,var.equal=FALSE)$p.value
tw.pvalue


#Question 3----------------------------------------------------------------
set.seed(123)
long <- rcauchy(1000,location=0,scale=1)
qqnorm(long,main="Long Tailed Distribution")
qqline(long)

set.seed(123)
short <- runif(1000,min=0,max=2)
qqnorm(short,main="Short Tailed Distribution")
qqline(short)

set.seed(123)
left <- rbeta(1000,2,0.5,ncp=2)
qqnorm(left,main="Left Skewed Distribution")
qqline(left)

set.seed(123)
right <- rexp(1000,rate=2)
qqnorm(right,main="Right Skewed Distribution")
qqline(right)


#Question 4----------------------------------------------------------------
xA<- -0.255- (-0.324)
xB<- -0.213- (-0.185)
xC<- -0.19- (-0.299)
xD<- -0.185- (-0.144)
xE<- -0.045- (-0.027)
xF<- -0.025- (-0.039)
xG<- -0.015- (-0.264)
xH<- 0.003- (-0.077)
xI<- 0.015- (-0.017)
xJ<- 0.02- (-0.169)
xK<- 0.023- (-0.096)
xL<- 0.04- (-0.33)
xM<- 0.04- (-0.346)
xN<- 0.05- (-0.191)
xO<- 0.055- (-0.128)
xP<- 0.058- (-0.182)
x=c(xA,xB,xC,xD,xE,xF,xG,xH,xI,xJ,xK,xL,xM,xN,xO,xP)
mean(x)
sd(x)
t.gamma<-0.05*4/sd(x)

t.crit <- qt(1-0.05/2,15)
t.power <- 1-pt(t.crit,15,ncp=t.gamma)+pt(-t.crit,15,ncp=t.gamma)
t.power

n <- seq(16,100)
t.crit <- qt(1-0.05/2,n-1)
t.gamma <- 0.05*sqrt(n)/sd(x)
t.power <- 1-pt(t.crit,n-1,ncp=t.gamma)+pt(-t.crit,n-1,ncp=t.gamma)
plot(n,t.power)
abline(h=0.9,col='red')
abline(v=80,col='blue')


power.t.test(n = NULL, delta = 0.05, sd = sd(x), sig.level = 0.05,
             power = 0.9, type = "paired")


#Question 5----------------------------------------------------------------
y <- c(256, 159, 149, 54, 123, 248)
x <- c("A", "A", "A", "B", "B", "B")
t.obs <- abs(mean(y[x=="B"]) - mean(y[x=="A"])) /
  (sqrt(1/3)*sqrt(var(y[x=="B"]) + var(y[x=="A"])))
t.obs

n.combs <- choose(6,3)
all.combinations <- combn(6,3)
t.null <- numeric(n.combs)
for(i in 1:n.combs){
B.indices <- all.combinations[,i]
t.null[i] <- abs(mean(y[B.indices ]) - mean(y[-B.indices ])) /
  (sqrt(1/3)*sqrt(var(y[B.indices]) + var(y[-B.indices])))
}
mean(t.null >= t.obs)
