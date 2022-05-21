# File: Statistics 502 R Lab 1
# Based off material provided by previous TAs

# Lines beginning with the hash mark "#" are comments. They will not do anything if sent to the interpreter.

#########################
#Vectors
#########################

# The basic data structure in R is the vector.
# A vector contains elements of the same type. In R, you do not need to explicitly declare a type. 
# We will see this in the following examples.

#To construct an empty vector, use the constructor c()
e <- c()
e

x <- c(1,2,3,4)        # numeric vector
x

y <- c(TRUE, FALSE)    # logical vector
y

z <- c("hey","hello")     # character vector
z

# mixing types in vector coerces to common type.
w <- c(1, "hey", TRUE)
w

rm(list = ls(all = TRUE)) #clear environment of definitions from previous/current R session

#re-create x and y vector
x <- c(1,2,3,4)    
y <- c(TRUE, FALSE)

class(x)          #determine what type of elements are in the vector
typeof(x)
is.numeric(x)     #determine if "x" is a numeric variable
is.vector(x)      #determine if "x" is a vector
is.vector(5)      #pretty much everything in R is a vector

#you can add metadata to vectors to assist with element look-ups
names(x) <- c("one", "two", "three", "four")
x

#R provides multiple ways to extract elements from vectors.
#The following lines all extract the first element of the vector "x"
x[1]
x["one"]
x[-(2:4)] 
x[x==1]

# what do you think these will output?
2:4
-(2:4)
x==1

#R provides many built in functions to compute statistics.
mean(x)         #average of vector elements
sum(x)          #sum of vector elements 
length(x)       #number of elements in vector
prod(x)         #product of vector elements
var(x)          #variance of vector elements 
sd(x)           #standard deviation of vector elements
min(x)          #minimum element in vector.
max(x)          #maximum element in vector.

mean(y)         #note applying functions to a logical vector doesn't cause an error.

# what will these lines output?
mean(x[x != 4]) #logical vectors can be used as indices for element look up.
mean(x != 4)    #compare this to the previous line.

q <-  c(NA, NA, 5,4,NA,7)
is.na(q)
q[!is.na(q)] #use logical vector as index

#Many operations are vectorized in R
x * 10 
x^2
x + c(1,1,1,1)

#R provides many ways to construct vectors. Several examples follow.
m <- seq(1,11, by=2)
m

m <- 1:10
m

m <- rep(c(0,1), each=2)
m

m <- rep(c(0,1), times=4)
m

m <- rep(c(0,1), each=2, times=4)
m

#Logical vectors support logical operations
A <- c(TRUE, FALSE, FALSE)
Z <- c(FALSE, FALSE, TRUE)
A & Z
A | Z
A == Z
A != Z
!A

#One common use for R is to conduct simulation studies.
#In order to guarantee that running the code multiple times produces the same results
#use the "set.seed()" function to set the pseudo-random-number generator to a specific state.
set.seed(9181)

#Not sure how to use a function?  Use the `?' command:
?sample

#Here we generate a sample with replacement of size 30 according to the specified weights.
samp <-  sample(c("a","z"), 
                size=30, 
                replace=T, 
                prob=c(0.75,0.25))
samp

#rnorm() is used here to sample 1000 pseudo-random-numbers from a Normal(0,1) distribution
norm.samp <-  rnorm(1000,mean=0,sd=1)
hist(norm.samp, 
     col="blue", 
     xlab="Normal Samples", 
     main="Histogram of Normal Samples", 
     freq=F)

#"lines()" is used when you want to super-impose a curve on a plot
#"dnorm()"corresponds to the normal density function
x.grid <-  seq(-4,4, by=0.01)
lines(x.grid, dnorm(x.grid, mean=0, sd=1), lwd=3) 

#########################
#Data frames & matrices
#########################
#Beyond vectors, some common data structures you will encounter in R are matrices and data frames.
M = matrix(1:12, nrow=4, ncol=3, byrow=T)
M
#transpose the matrix
t(M)
#lookup sub-matrices
M[1:2,1:2]
M[1:2,-c(3:4)]

# load data frame
data(mtcars)   #load a built in data set.
# look at the data and get some summary statistics
head(mtcars)
summary(mtcars)
#mtcars is a data frame
is.data.frame(mtcars)
#can extract columns of data frames as vectors using "$" notation.
new.vec <- mtcars$cyl
new.vec
is.vector(new.vec)
#can also extract columns from the data frame using similar syntax to vectors.
mtcars[,"cyl"]
mtcars[,2]
mtcars[,-c(1,3:11)]
mtcars[,colnames(mtcars) == "cyl"]

#############
#Graphics
#############
plot(mtcars$cyl, 
     mtcars$mpg,
     col="brown",
     pch=19,
     xlab="Cylinders in Car",
     ylab="Miles per Gallon",
     main="Scatterplot of MPG vs. Cylinders")

boxplot(mpg ~ cyl,
        data=mtcars,
        xlab="Cylinders in Car",
        ylab="Miles per Gallon",
        main="Boxplot")

#########################
#Using external data
#########################

#load an external data set using read.table(). compare this function with read.csv() or fread()
easy = read.table("easy.txt", header=TRUE)

# look at the data and get some summary statistics
head(easy)
summary(easy)

#compare different methods of accessing columns
easy[, 1]
easy$x1
easy[, "x1"]
easy[, which(colnames(easy) == "x1")]

#plots
pairs(easy)
boxplot(easy)

##########################
#Simulation Example
##########################
# Let's approximate the sampling distribution of the sample mean/median 
# from 100 Uniform(0, 1) observations We will use the variable NITERS to 
# determine the number of iterations
NITERS <- 1e4

#create vectors to store simulation results
mean.store <- median.store <- rep(NA,NITERS)
length(mean.store)
unique(mean.store)

#set the seed to ensure we get the same results if we re-run code
set.seed(72731)
for (i in seq(NITERS)) {
  samps <- runif(100, 0, 1)
  mean.store[i] <- mean(samps)
  median.store[i] <- median(samps)
}

hist(mean.store, col="green")
mean(mean.store)
sd(mean.store)
sqrt(1/12) / sqrt(100) 

hist(median.store,
     col="brown")

mean(median.store)
sd(median.store)
