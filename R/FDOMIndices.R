#'@export

#These functions are designed to work with the output of PARAFAC.cube.design()
#They can't be used directly with fluorescence scan unless they are stacked in a array (Ex, Em, Samples)

#Descriptions of the indices may be found in Gabor et al. Fluorescence Indices and Their Interpretation


#Fluorescence index from McKnight et al (2001) and Cory and McKnight (2005)
#cube is the full output of PARAFAC.cube.design(), list of 5 elements
#cor is a logical parameter meaning that fluorescence values are corrected for the instrument, default is TRUE

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


BIX <- function(cube)
{
  
  return(output)
}



HIX <- function()
{
  
  return(output)
}






