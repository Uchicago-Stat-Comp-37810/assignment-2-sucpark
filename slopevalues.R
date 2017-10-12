source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/likelihood.R")

slopevalues <- function(x)
{
  return(likelihood(c(x, trueB, trueSd))) #Return all seta 
}