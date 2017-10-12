source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/likelihood.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/slopevalues.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/prior.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/proposalfunction.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/posterior.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/run_mh_mcmc.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/Summary.R")

#Creating test data

trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

par(mfrow=c(1,1))
plot(x,y, main="Test Data")

# Example: plot the likelihood profile of the slope a

slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues ) #slopevalues function works for each element in the prior vector.

plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")

#The MCMC
######## Metropolis algorithm ################

startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000 # Discard 5000 data when computing acceptance.
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #Compute the rejection rate of proposal function

### Summary: #######################
Summary(chain, burnIn,trueA,trueB,trueSd)
# for comparison:
summary(lm(y~x))
