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