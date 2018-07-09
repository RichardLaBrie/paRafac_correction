#'@title Calculate Sr for CDOM samples
#'
#'@description This function reads any number of CDOM files and calculate their Sr.

#FileSelect is a logical argument for the number of files to load. Default is FALSE, meaning that the function
  #will automatically load and calculate S for all CDOM files "./data/CDOM"
  #If TRUE, you get to choose the files you want
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

Sr <- function(FileSelect = F, wl1.1 = 275, wl1.2 = 295, wl2.1 = 350, wl2.2 = 400, By = 1, skip = 1)
{

  if(!FileSelect) file.dir = list.files("./data/CDOM")
  if(FileSelect) file.dir = choose.files("./data/CDOM")
  wl.num = seq(wl1.1, wl1.2, By)
  wl.denom = seq(wl2.1, wl2.2, By)
  Sr = matrix(0, nrow = length(file.dir), ncol=1)
  rownames(Sr) = file.dir
  
  for(i in 1:length(file.dir)) 
  {
    CDOM = read.table(paste0("./",data.file,"/CDOM/",file.dir[i]), skip = skip, header = skip + 1, sep=",")
    WL = CDOM[,1]
    
    Abs.num = sapply(wl.num, function(x){return(subset(CDOM[,2], WL == x))})
    Abs.denom = sapply(wl.denom, function(x){return(subset(CDOM[,2], WL == x))})
    
    #Calculate S for wl = 275 and wl = 350, based on Helms 2008
    Slope.num = coef(lm(log(Abs.num) ~ wl.num))[2]
    Slope.denom = coef(lm(log(Abs.denom) ~ wl.denom))[2]
    #Calculate the slope ratio
    Sr[i,1] = Slope.num / Slope.denom
  }
  
  #Return result
  return(Sr)
  
}
