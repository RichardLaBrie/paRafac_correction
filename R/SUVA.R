#'@title Calculate SUVA indice
#'
#'@description This function reads any number of CDOM files and calculate SUVA.
#'You have to manually load DOC file for SUVA
#'Wavelengths chosen based on Helms 2008
#'Calculate spectral slope with a reference wavelength
#'The model use a nonlinear least square fitting routine
#'The equation is abs = abs0 * exp(-S(wavelength - ref.wavelength)) + K as in Markager & Vincent 2000
#'where abs is the absorption coef, abs0 is the reference absorption coef at wavelenth and ref.wavelength
#'S is the slope and K is an additional background parameter to allow baseline to shift 

#'@param FileSelect is a logical parameter to allow you which file to load. Default is FALSE, meaning that all files in /CDOM
#'will be loaded

#DOC must be in mg/L, with sample names as row names and one column with the values
#If DOC is in µM, change "unit" to "uM"
#Column one on CDOM is wavelength (nm) and column two is absorption coefficient
#wl is set at 254 for SUVA254 but can be change to obtain any desired wavelength
#skip is the number of lines to skip before getting the header in CDOM files

#'@export

SUVA <- function(data.file = "data", FileSelect = F,DOC, wl = 254, unit = "mg/L", skip = 1, name="SUVAMatches")
{
  if(unit == "uM") DOC = DOC * 12 / 1000
  if(!FileSelect) file.dir = list.files("./data/CDOM")
  if(FileSelect) file.dir = choose.files("./data/CDOM")
  
  #Create the .csv file to make sure matches were done correctly
  file.create("..\\SUVAMatches.csv")
  write.table(t(c("Target", "Source", "Distance")), "..\\SUVAMatches.csv", append=T, sep=",", col.names = F)

  #Ordering CDOM files based on the order of DOC sample names
  index = sapply(rownames(DOC), selectMinStringDist, source = file.dir, name=name)
  file.dir = file.dir[index]
  
  #Calculate SUVA at wavelength = wl
  SUVA254 = matrix(0, nrow = length(file.dir))
  for(i in 1:length(file.dir)) 
  {
    Abs = read.table(paste0("./",data.file,"/CDOM/",file.dir[i]), skip = skip, header = skip + 1, sep=",")
    a254 = Abs[,2][Abs[,1] == wl]
    SUVA254[i,1] = a254 / DOC[i]
  }
  
  #Name column and rows
  rownames(SUVA254) = rownames(DOC)
  colnames(SUVA254) = "SUVA"
  #Return the values
  return(SUVA254)
}
