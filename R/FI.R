#'@title Calculate FI
#'
#'@description This function works with the output of PARAFAC.cube.design and calculate FI.
#'@references Fluorescence index (FI) from McKnight et al (2001) and Cory and McKnight (2005)
#'
#'


FI <- function(cube, cor = T)
{
  if(!is.list(cube)) return(print("You need the complete list of PARAFAC.cube.design output"))
  
  #Define wavelength to be used. Emission changes if the data are corrected for the instrument
  if(cor)
  {
    em1 = 470
    em2 = 520
  }
  else
  {
    em1 = 450
    em2 = 500
  }
  ex = 370
  
  output = matrix(0, nrow = cube[[5]])
  rownames(output) = cube[[2]]
  colnames(output) = "FI"
  for(k in 1:cube[[5]])
  {
    Fluo.em1 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em1), k]
    Fluo.em2 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em2), k]
    output[k,1] = Fluo.em1 / Fluo.em2
  }
  
  return(output)
}