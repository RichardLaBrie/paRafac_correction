#'@title Calculate Sr for CDOM samples
#'
#'@description This function reads any number of CDOM files and calculate their Sr.

#data is the output of SpectralSlope.R
#wl1.1 is the desired wavelength for the spectral ratio numerator
#wl1.0 is the reference wavelength for the spectral ratio numerator
#wl2.1 is the desired wavelength for the spectral ratio denominator
#wl2.0 is the reference wavelength for the spectral ratio denominator
#By is the step between each wavelength, default is 1
#skip is the number of lines to skip before getting the header in CDOM files
#Logfit is a logical parameter to compute the slope ratio using log-transform data to calculate the slopes
  #Default is TRUE. If FALSE, the function does two fit routine for each subsection of the spectra
  #Values differ more as the ratio diminish


#'@export

Sr.spectralslope <- function(data, wl1.1 = 275, wl1.2 = 295, wl2.1 = 350, wl2.2 = 400, wl0 = 375)
{
  if(class(data) != "SpectralSlope") return(Print("data must come from the SpectralSlope function"))
  output = matrix(0,nrow=dim(data)[1])
  colnames(output) = "Sr"
  rownames(output) = rownames(data)
  for(i in 1:dim(data)[1])
  {
  temp1 = ((data[i,1] * exp(-1*data[i,2] * (wl1.2-wl0)) + data[i,4]) - (data[i,1] *
          exp(-1*data[i,2] * (wl1.1-wl0)) + data[i,4])) / (wl1.2-wl1.1)
  
  temp2 = ((data[i,1] * exp(-1*data[i,2] * (wl2.2-wl0)) + data[i,4]) - (data[i,1] *
          exp(-1*data[i,2] * (wl2.1-wl0)) + data[i,4])) / (wl2.2-wl2.1)
  output[i,1] = temp1/temp2
  }
  
  return(output)
}
