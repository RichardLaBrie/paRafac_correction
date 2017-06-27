#Wavelengths chosen based on Helms 2008
#Calculate spectral slope of wavelength (1) with reference wavelength (0)
#The order of the argument is CDOM file, wavelength (1), reference wavelength (0)

SpectralSlope <- function(CDOM, wl1, wl0)
{
  #Find absorption coefficient at relevant wavelength
  a0 = CDOM[,2][CDOM[,1] == wl0]
  a1 = CDOM[,2][CDOM[,1] == wl1]
  
  #Calculate log S
  Slog = -log(a0) / ((wl0 - wl1) * log(a1))
  
  #Back calculate S to use it in Sr
  S = exp(Slog)
  return(S)
}

#wl1.1 is the desired wavelength for the spectral ratio numerator
#wl1.0 is the reference wavelength for the spectral ratio numerator
#wl2.1 is the desired wavelength for the spectral ratio denominator
#wl2.0 is the reference wavelength for the spectral ratio denominator
#skip is the number of lines to skip before getting the header in CDOM files

Sr <- function(wl1.1 = 275, wl1.0 = 295, wl2.1 = 350, wl2.0 = 400, skip = 1)
{
  setwd("./data/CDOM")
  file.dir = list.files()
  
  Sr = 0
  
  for(i in 1:length(file.dir)) 
  {
    CDOM = read.table(file.dir[i], skip = skip, header = skip + 1, sep=",")
    #Calculate S for wl = 275 and wl = 350, based on Helms 2008
    S275 = SpectralSlope(CDOM, wl1.1, wl1.0)
    S350 = SpectralSlope(CDOM, wl2.1, wl2.0)
    
    #Calculate the slope ratio
    Sr[i] = S275 / S350
  }

  #Go back to main folder
  setwd("..")
  setwd("..")
  #Return result
  return(Sr)
}




#DOC must be in mg/L, with sample names as row names and one column with the values
#If DOC is in µM, change "unit" to "uM"
#Column one on CDOM is wavelength (nm) and column two is absorption coefficient
#wl is set at 254 for SUVA254 but can be change to obtain any desired wavelength
#skip is the number of lines to skip before getting the header in CDOM files

SUVA <- function(DOC, wl = 254, unit = "mg/L", skip = 1, name="SUVAMatches")
{
  if(unit == "uM") DOC = DOC * 12 / 1000
  setwd("./data/CDOM")
  file.dir = list.files()
  
  #Create the .csv file to make sure matches were done correctly
  file.create("..\\SUVAMatches.csv")
  write.table(t(c("Target", "Source", "Distance")), "..\\SUVAMatches.csv", append=T, sep=",", col.names = F)

  #Ordering CDOM files based on the order of DOC sample names
  index = sapply(rownames(DOC), selectMinStringDist, source = file.dir, name=name)
  file.dir = file.dir[index]
  
  #Calculate SUVA at wavelength = wl
  SUVA254 = list()
  for(i in 1:length(file.dir)) 
  {
    Abs = read.table(file.dir[i], skip = skip, header = skip + 1, sep=",")
    a254 = Abs[,2][Abs[,1] == wl]
    SUVA254[i] = a254 / DOC[i]
  }
  
  #Return to main folder
  setwd("..")
  setwd("..")
  #Create a matrix with the values
  SUVA254 = as.matrix(unlist(SUVA254))
  #Name column and rows
  rownames(SUVA254) = rownames(DOC)
  colnames(SUVA254) = "SUVA"
  #Return the values
  return(SUVA254)
}

