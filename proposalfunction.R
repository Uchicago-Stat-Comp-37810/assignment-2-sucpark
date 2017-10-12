proposalfunction <- function(param)
{
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3))) #probability from normal dist
}