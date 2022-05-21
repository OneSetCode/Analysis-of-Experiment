##### Lab 3

## Statistical Power:

# Say we conduct a z-test for our wheat example from earlier at the 5% significance level.  
# How many subplots would we need to have at least a 90% chance of discerning a 
# difference in means of 1 unit?  Assume wheat yields are normal, nA=nB=n/2, and the 
# variances of the two fertilizer yield distributions are equal (sigmaSq=30).

alpha=.05 
sigmaSq = 30
sigma=sqrt(sigmaSq)
N = 1000 
thresh = qnorm(1-alpha/2)
probs = numeric(N)


for(i in 1:N) {
  n=2*i
  probs[i] = 1 - pnorm(thresh, mean=sqrt(n)/(2 * sigma), sd=1) +
    pnorm(-thresh, mean=sqrt(n)/(2 * sigma), sd=1)
}
# To see why the mean is equal to above, note that
# n_A = n_B = n/2; the mean shift is delta / sigma * (sqrt( 1 /n_A + 1/n_B))
# with delta = 1, sqrt(1/n_A + 1/ n_B) = 2/sqrt(n)

# True difference of 2 units. 
probs.2 = numeric(N)
for(i in 1:N){
  n = 2 * i
probs.2[i] = 1 - pnorm(thresh, mean = 2 * sqrt(n)/(2 * sigma), sd=1) +
  pnorm(-thresh, mean= 2 * sqrt(n)/(2 * sigma), sd=1)
}

# True difference of point 5 units. 
probs.Point5 = numeric(N)
for(i in 1:N){
  n = 2 * i
  probs.Point5[i] = 1 - pnorm(thresh, mean = 0.5* sqrt(n)/(2 * sigma), sd=1) +
    pnorm(-thresh, mean= 0.5 * sqrt(n)/(2 * sigma), sd=1)
}

# Now let's compare the plots to see how much data we need to see a difference in means.
plot(probs)
lines(probs.2, col = "red")
lines(probs.Point5, col = "blue")



probs

# the answer:
ind = match(1, probs >= .9)
ind
n=ind*2
prob=probs[ind]
n
prob

## Apply functions: R is notoriously slow when using "for" and "while" loops.  Instead, you should 
## use "apply" functions when possible for faster computation time.  Examples of apply functions are:
# sapply
# lapply
# apply
# mapply
# sweep (sort of)
# others

## sapply

# first we define a weird function
removeNAs = function(x, ifNA=0) {
  if(is.na(x))
    ifNA
  else
    x
}

# but this doesn't work on a vector input:
removeNAs(c(1, NA, 3, 4, 5))
removeNAs(NA)
removeNAs(1)

# How can we get around this? Use sapply:
testDat = c(1, NA, 3, 4, 5)
sapply(testDat, removeNAs)

# we can also set function inputs with sapply:
sapply(testDat, removeNAs, ifNA=mean(testDat, na.rm=TRUE))


## apply:

# say we have a random dataset and want to estimate the SD of each variable:
set.seed(123)
myData = data.frame(matrix(rnorm(100), nrow=10, ncol=10))

# using the sd function lots of times is tedius.  Instead we use apply:
apply(myData, 2, sd)

# we can also set inputs to the sd function in apply:
myData[1,2] = NA
apply(myData, 2, sd)
apply(myData, 2, sd, na.rm = TRUE)

?apply
# What is MARGIN?

#If the SDs of the values in each row in the dataset was important, we could also get those.
#Row is the first index, column is the second.  Hence, for MARGIN=1, we get the SD for each row:
apply(myData, 1, sd, na.rm=TRUE)

## sweep:

# Say we want to center our dataset so that each variable has mean zero.  Sweep is the function for this:
colAvgs = apply(myData, 2, mean, na.rm=TRUE)
cntrData = sweep(myData, 2, colAvgs, "-")

# perhaps we also want to standardize each column so that their standard deviations are 1.  We can also use
# sweep for this:
colSDs = apply(myData, 2, sd, na.rm=TRUE)
stdData = sweep(cntrData, 2, colSDs, "/")

# the data now look ~standard normal:
hist(as.matrix(stdData))

