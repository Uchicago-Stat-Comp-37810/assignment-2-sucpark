source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/proposalfunction.R")
source("~/Desktop/2017-Autumn/Github/assignment-2-sucpark/posterior.R")

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