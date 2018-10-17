#'@title Calculate BIX
#'
#'@description This function works with the output of PARAFAC.cube.design and calculate BIX.
#'@references Biological index (BIX) from Wilson and Xenopoulos (2009)


BIX <- function(cube)
{
  if(!is.list(cube)) return(print("You need the complete list of PARAFAC.cube.design output"))
  
  em1 = 380
  em2 = 420
  em3 = 436
  ex = 310
  
  output = matrix(0, nrow = cube[[5]])
  rownames(output) = cube[[2]]
  colnames(output) = "BIX"
  for(k in 1:cube[[5]])
  {
    Fluo.em1 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em1), k]
    Fluo.em2 = cube[[1]][which(cube[[3]]==ex), c(which(cube[[4]]==em2):which(cube[[4]] == em3)), k]
    Fluo.em2 = c(Fluo.em2, mean(c(Fluo.em2[length(Fluo.em2) - 1], Fluo.em2[length(Fluo.em2)])))
    output[k,1] = Fluo.em1 / max(Fluo.em2)
  }
  
  return(output)
}
