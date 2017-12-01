#'@title Substract a corrected EEM from another
#'
#'@description This function select 2 EEMs based on their name and subtract one from another
#'@param cube is the output of the main function PARAFAC.cube.design()
#'@param diminuende is the sample you want to subtract from (Ex. a - b = c, a is the diminuende)
#'@param diminuteur is the sample you want to subtract (Ex. a - b = c, b is the diminuteur)
#'@param Exact_ende is a logical parameter for diminuende name. Default is TRUE (Ex. Bunker != BunkerIPA50x)
#'If paramter is set to FALSE, Bunker = BUNKERIPA50x. Beware of potential confusion if names are similar!
#'@param Exact_teur is a logical parameter for diminuteur name. Default is TRUE (Ex. LTM != LTMGose50x)
#'@param zlim is the intensity to plot the EEMs. Negative and values outside of scale bar will be white
#'If paramter is set to FALSE, LTM = LTMGose50x. Beware of potential confusion if names are similar!

#'@export

DeltaEEM <- function(cube, diminuende, diminuteur, exact_ende = T, exact_teur = T, zlim = c(-1,1))
{
  
  if(exact_ende) #Select diminuende matrix in cube
  {
    X = cube[[1]][,,which(cube[[2]]=="diminuende")]
  }
  else
  {
    X = cube[[1]][,,grep(pattern = paste(diminuende), cube[[2]])]
  }
  
  if(exact_teur) #Select diminuteur matrix in cube
  {
    Y = cube[[1]][,,which(cube[[2]]=="diminuteur")]
  }
  else
  {
    Y = cube[[1]][,,grep(pattern = paste(diminuteur), cube[[2]])]
  }
  output = X - Y
  
  #Print the plot
  filled.contour(cube[[3]], cube[[4]], output, color.palette = myPalette,
                 xlab = "Excitation (nm)", ylab = "Emission (nm)",
                 main = bquote(.(diminuende) - .(diminuteur)), zlim = zlim, nlevels = 50)
  
  #Return the matrix values
  return(output)
}