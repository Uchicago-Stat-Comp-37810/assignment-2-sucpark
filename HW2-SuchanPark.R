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

#Derive the likelihood funtion from the model

likelihood <- function(param)
{
  a = param[1] #Possible a is from 3 to 7
  b = param[2]
  sd = param[3]
  
  pred = a*x + b #new y
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) # Error follow normal dist. So difference between y and pred is based on normal dist.
  sumll = sum(singlelikelihoods) #Because of Log transition
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x)
{
  return(likelihood(c(x, trueB, trueSd))) #Return all seta 
}
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues ) #slopevalues function works for each element in the prior vector.

plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")
###############################################################

#Defining the prior

prior <- function(param)
{
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior) #log
}

#The posterior

posterior <- function(param)
{
  return (likelihood(param) + prior(param))
}

#The MCMC
######## Metropolis algorithm ################

proposalfunction <- function(param)
{
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3))) #probability from normal dist
}

run_metropolis_MCMC <- function(startvalue, iterations)
{
  chain = array(dim = c(iterations+1,3)) # Make an empty array iterations+1 X 3 (3 = number of start value)
  chain[1,] = startvalue # First row is a startvalue.
  for (i in 1:iterations)
  {
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,])) #Use exp() because of Log transition, originally, it is p2/p1
    
    if (runif(1) < probab)
    {
      chain[i+1,] = proposal #update
    }else
    {
      chain[i+1,] = chain[i,] #maintain
    }
  }
  return(chain)
}

startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000 # Discard 5000 data when computing acceptance.
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #Compute the rejection rate of proposal function

### Summary: #######################

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],nclass=30, main="Posterior of a", xlab="True value = red line" )
abline(v = mean(chain[-(1:burnIn),1]))
abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = trueB, col="red" )
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),3]) )
abline(v = trueSd, col="red" )

plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a")
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b")
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd")
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))