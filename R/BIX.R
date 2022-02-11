#'@title Calculate BIX
#'
#'@description This function works with the output of PARAFAC.cube.design and calculate BIX.
#'@references Biological index (BIX) from Huguet et al. 2009


BIX <- function(cube)
{
  if(!is.list(cube)) return(print("You need the complete list of PARAFAC.cube.design output"))
  
  em1 = 380
  em2 = 430
  ex = 310
  
  output = matrix(0, nrow = cube[[5]])
  rownames(output) = cube[[2]]
  colnames(output) = "BIX"
  for(k in 1:cube[[5]])
  {
    Fluo.em1 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em1), k]
    Fluo.em2 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em2), k]
    output[k,1] = Fluo.em1 / Fluo.em2
  }
  
  return(output)
}
