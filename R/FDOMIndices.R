#'@title Generic functions for commonly used FDOM indices
#'
#'@description This is a warp up function to calculate FI, BIX and HIX based on the output of PARAFAC.cube.design()
#'@references Fluorescence index (FI) from McKnight et al (2001) and Cory and McKnight (2005)
#'@references Biological index (BIX) from Wilson and Xenopoulos (2009)
#'@references Humification index (HIX) from Ohno et al (2002)

#'@param cube is the output of PARAFAC.cube.design()
#'@param cor is a logical argument for instruments corrections applied to EEMs. Default is TRUE
#'@param rel is a logical argument to scale the values of HIX

#'@export

#These functions are designed to work with the output of PARAFAC.cube.design()
#They can't be used directly with fluorescence scan unless they are stacked in a array (Ex, Em, Samples)

#Descriptions of the indices may be found in Gabor et al. Fluorescence Indices and Their Interpretation
#Or in Gao et al. Spectral characteristics of DOM in various soils throughout China




FDOMIndices <- function(cube, cor = T, rel = F)
{
  output = matrix(0, nrow = cube[[5]], ncol=3)
  rownames(output) = cube[[2]]
  colnames(output) = c("FI","BIX","HIX")
  output[,1] = FI(cube, cor)
  output[,2] = BIX(cube)
  output[,3] = HIX(cube, rel)
  return(output)
}
  
