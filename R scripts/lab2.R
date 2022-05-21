# File: Statistics 502 R Lab 2
# Based off material provided by previous TAs

#Let's start by discussing lab2-problem
n <- 40                    #n represents the sample size of 40
pS.G1 <- 0.8            #Probability(Smoker=1 | Genetic mutation = 1)
pS.G0 <- 0.2            #Probability(Smoker=1 | Genetic mutation = 0)

##################################################
#Begin by implementing a single observational study sim.
##################################################
set.seed(234)

#In the observational study, Probability(Genetic mutation = 1) = 1/2
#So, in a specific simulation we need to flip 40 coins and see who gets the mutation in that iteration
#Of the simulation. Here we simulate a single observational study
G <- rbinom(n,1,0.5)
G

nG <- sum(G) #This is the number of people in the sample who have the mutation
nG

#In the observational study, smoking status depends on the genetic mutation through the conditional probabilities.
#So we will need to determine smoker status according to those distributions.
#First, we make a vector to store smoker status
S <- rep(0,n)
S

S[G==1] <- rbinom(nG,1,pS.G1) #This determines smoker status for people who have the genetic mutation
S
S[G==1]
S[G==0]

S[G==0] <- rbinom(n-nG,1,pS.G0) #This determines smoker status for people who don't have the genetic mutation.
S
S[G==1]
S[G==0]

#Finally, we determine the lung health of the people in the study using the relation L=2G+eps, and compute the correlation
#between lung healh and smoker status
L <- 2*G + rnorm(n,0,1)
cor(L,S)

##################################################
#Here we simulate the observational study.
##################################################
set.seed(1234)
NSIM <- 1e5
obs.cors <- rep(0,NSIM) #make a vector to store the correlation from each simulation
for(i in 1:NSIM){
  G <- rbinom(n,1,0.5) 
  nG <- sum(G)
  S <- rep(0,n)
  S[G==1] <- rbinom(nG,1,pS.G1)
  S[G==0] <- rbinom(n-nG,1,pS.G0)
  ## Alternatively, write a for loop from 1 to 40
  L <- 2*G + rnorm(n,0,1)
  obs.cors[i] <- cor(L,S) #store the correlation from this simulation round in the vector.
}
summary(obs.cors)
hist(obs.cors, xlab="Correlation", main="Histogram of correlations in an observational study")


################################################################################################
#Begin by implementing a single randomized controlled  study sim.
################################################################################################

#Next, lets simulate a single randomized controlled experiment
#The probability of the random mutation remains unchanged
G <- rbinom(n,1,0.5)
G

nG <- sum(G)
nG

#In the randomized controlled experiment, we now are going to randomly assign smoker status to half the sample.
all <- seq(1,40) #The vector all represents the 40 people in the study.
all

#Next, we take sample half the study population without replacement. This will be the smoking group.
smoking.group <- sample(all, 20,replace=FALSE)
smoking.group

#The smoking status of an individual in the study is now determined by the random sample.
S <- as.numeric(all %in% smoking.group)
S

#We still determine lung health the same way
L <- 2*G + rnorm(n,0,1)
cor(L,S)

##################################################
#Here we simulate the randomized experiment
##################################################
NSIM <- 1e5
exp.cors <- rep(0,NSIM) #vector to store simulation results
for(i in 1:NSIM){
  G <- rbinom(n,1,0.5)
  nG <- sum(G)
  all <- seq(1,40)
  smoking.group <- sample(all, 20)
  S <- as.numeric(all %in% smoking.group)
  ## Alternatively: smoking <- c(rep(0,20), rep(1,20)); S <- sample(smoking)
  L <- 2*G + rnorm(n,0,1)
  exp.cors[i] <- cor(L,S)
}
summary(exp.cors)
hist(exp.cors, xlab="Correlation", main="Histogram of correlations in an randomized experiment")

##############################################################
#Next, let's go over lecture example, chapter 2, page 21
##############################################################
y <- c(26.9,11.4,26.6,23.7,25.3,28.5,14.2,17.9,16.5,21.1,24.3,19.6) #wheat yields
x <- c("B","A","B","A","B","B","B","A","A","A","B","A") #fertilizer assignment
g.obs <- abs(mean(y[x=="B"])-mean(y[x=="A"])) #g.obs is the observed test statistic
g.obs

#construct the randomization distribution
n.combs <- choose(12,6)
n.combs
all.combinations <- combn(12,6)
all.combinations[,1:4] ## seeing what object looks like
dim(all.combinations)
#null hypothesis: no treatment effect
g.null <- numeric(n.combs)
for(i in 1:n.combs){
    A.indices <- all.combinations[,i]
    g.null[i] <- abs(mean(y[A.indices])-mean(y[-A.indices]))
}
mean(g.null>=g.obs)
hist(g.null)

#rather than construct the randomization distribution, can take the monte carlo approach and simulate.
S <- 1e4
set.seed(1) ## for reproducibility
g.null <- numeric(S)
for(s in 1:S){
    xsim <- sample(x)
    g.null[s] <- abs(mean(y[xsim=="B"])-mean(y[xsim=="A"]))
}
mean(g.null>=g.obs)
hist(g.null)

##############################################################
#What happens if sample is bigger?
##############################################################
set.seed(2341)
y1 <- round(rgamma(20,shape=10,scale=2))
y2 <- round(rgamma(20,shape=11,scale=2))
y <- c(y1,y2)
x <- c(rep("A",20),rep("B",20))

g.obs <- abs(mean(y[x=="B"])-mean(y[x=="A"]))
g.obs

#what happens if we try all combinations?
n.combs <- choose(40,20)
n.combs 
all.combinations <- combn(40,20)

#so, the monte-carlo approach necessary. how many simulations do we need to run? can experiment.
S1 <- 1e4
S2 <- 1e5
S3 <- 1e6
set.seed(3344) # for reproducibility

#rather than copy paste the code, can control simulation by defining a function.
runSim <- function(NITERS,DATA,GROUP.LABEL) {
    #name of function: runSim
    #argument list: NITERS, DATA, GROUP.LABEL
    #function copies in each argument and operates on them within the "{" and "}" symbol according to the type of object.
    g.null <- numeric(NITERS)
    for(s in 1:NITERS){
        xsim <- sample(GROUP.LABEL)
        g.null[s] <- abs(mean(DATA[xsim=="B"])-mean(DATA[xsim=="A"]))
    }
    return(g.null) #returns g.null with simulation values as function value
}

gn1 <- runSim(NITERS=S1,
              DATA=y,
              GROUP.LABEL=x)
mean(gn1 >= g.obs)
hist(gn1)

gn2 <- runSim(NITERS=S2,
              DATA=y,
              GROUP.LABEL=x)
mean(gn2 >= g.obs)

gn3 <- runSim(NITERS=S3,
              DATA=y,
              GROUP.LABEL=x)
mean(gn3 >= g.obs)

# Note on homeworks, code/discussion

#install.packatges("xtable")
library(xtable)
test = matrix(1:8, nrow=2)
colnames(test) = paste0("var", 1:4)
rownames(test) = c("MLEs", "SEs")
xtable(test)

