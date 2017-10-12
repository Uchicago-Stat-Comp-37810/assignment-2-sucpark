source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/likelihood.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/prior.R")

#The posterior

posterior <- function(param)
{
  return (likelihood(param) + prior(param))
}