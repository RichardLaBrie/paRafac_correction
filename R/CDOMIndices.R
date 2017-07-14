#'@export

#Wavelengths chosen based on Helms 2008
#Calculate spectral slope with a reference wavelength
#FileSelect is a logical argument for the number of files to load. Default is FALSE, meaning that the function
  #will automatically load and calculate S for all CDOM files "./data/CDOM"
  #If TRUE, you get to choose the files you want
#wl0 is the reference wavelength, default is 375 as in Stedmon, Markager and Kaas 2000
#From is the first wavelength needed to compute the exponential fit, default is 300 as in Stedmon, Markager and Kaas 2000
#To is the last wavelength needed, default is 650 as in Stedmon, Markager and Kaas 2000
#By is the step between each wavelength, default is 1
#skip is the number of lines to skip before getting the header in CDOM files

#The model use a nonlinear least square fitting routine
#The equation is abs = abs0 * exp(-S(wavelength - ref.wavelength)) + K as in Markager & Vincent 2000
#where abs is the absorption coef, abs0 is the reference absorption coef at wavelenth and ref.wavelength
#S is the slope and K is an additional background parameter to allow baseline to shift 


SpectralSlope <- function(data.file = "data", FileSelect = F, wl0 = 375, From = 275, To = 650, By = 1, skip = 1)
{
    setwd(paste0("./",data.file,"/CDOM"))
    if(!FileSelect) file.dir = list.files()
    if(FileSelect) file.dir = choose.files()
    #create sequence of desired wavelengths
    wl.x = seq(From, To, By)
    #Create the output matrix
    output = matrix(0,ncol = 4, nrow = length(file.dir))
    colnames(output) = c("Intercept", "Slope", "R2", "K")
    rownames(output) = file.dir
    
    #Compute the exponential fit for each CDOM file
    for(i in 1:length(file.dir))
    {
      Abs = read.table(file.dir[i], skip = skip, header = skip + 1, sep=",")
      WL = Abs[,1]
      a0 = Abs[which(Abs[,1] == wl0),2]
      Abs.y = sapply(wl.x, function(x){return(subset(Abs[,2], WL == x))})
      nls.temp <- nls(Abs.y ~ a0 * exp(-S * (wl.x - wl0)) + K, 
                  start = list(a0 = a0, S = 0.02, K = 0.02), #a0 is bounded between 0 and the highest observed value
                  lower = list(a0 = 0, S = 0, K = 0), #S is bounded between 0 and 1 otherwise the function grows
                  upper = list(a0 = max(Abs.y), S = 1, K = 3),
                  control = list(maxiter = 200, warnOnly = T),
                  algorithm = "port") #K is theoritically unbound [-Inf:Inf], but such values make no sense in this context. We've set the boundary to 0 and 3, which is the limit for absorption correction used with EEMs ()
      
      R2 <- 1 - sum((Abs.y - predict(nls.temp))^2) / (length(Abs.y) * var(Abs.y)) #Denominator is sum(y-mean(y))² which is variance(y) times length(y)
      
      output[i,1] = coef(nls.temp)[1]
      output[i,2] = coef(nls.temp)[2]
      output[i,3] = R2
      output[i,4] = coef(nls.temp)[3]
    }
    class(output) = "SpectralSlope"
    
    #Go back to main folder
    setwd("..")
    setwd("..")
    #Return the result
    return(output)
}

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

Sr <- function(FileSelect = F, wl1.1 = 275, wl1.2 = 295, wl2.1 = 350, wl2.2 = 400, By = 1, skip = 1)
{

  setwd("./data/CDOM")
  if(!FileSelect) file.dir = list.files()
  if(FileSelect) file.dir = choose.files()
  wl.num = seq(wl1.1, wl1.2, By)
  wl.denom = seq(wl2.1, wl2.2, By)
  Sr = matrix(0, nrow = length(file.dir), ncol=1)
  rownames(Sr) = file.dir
  
  for(i in 1:length(file.dir)) 
  {
    CDOM = read.table(file.dir[i], skip = skip, header = skip + 1, sep=",")
    WL = CDOM[,1]
    
    Abs.num = sapply(wl.num, function(x){return(subset(CDOM[,2], WL == x))})
    Abs.denom = sapply(wl.denom, function(x){return(subset(CDOM[,2], WL == x))})
    
    #Calculate S for wl = 275 and wl = 350, based on Helms 2008
    Slope.num = coef(lm(log(Abs.num) ~ wl.num))[2]
    Slope.denom = coef(lm(log(Abs.denom) ~ wl.denom))[2]
    #Calculate the slope ratio
    Sr[i,1] = Slope.num / Slope.denom
  }
  
  #Go back to main folder
  setwd("..")
  setwd("..")
  #Return result
  return(Sr)
  
}

Sr.spectralslope <- function(data, wl1.1 = 275, wl1.2 = 295, wl2.1 = 350, wl2.2 = 400, wl0 = 375)
{
  if(class(data) != "SpectralSlope") return(Print("data must come from the SpectralSlope function"))
  output = matrix(0,nrow=dim(data)[1])
  colnames(output) = "Sr"
  rownames(output) = rownames(data)
  for(i in 1:dim(data)[1])
  {
  temp1 = (data[i,1] * exp(-1*data[i,2] * (wl1.2-wl0) + data[i,4]) - (data[i,1] *
          exp(-1*data[i,2] * (wl1.1-wl0) + data[i,4])) / (wl1.2-wl1.1))
  temp2 = (data[i,1] * exp(-1*data[i,2] * (wl2.2-wl0) + data[i,4]) - (data[i,1] *
          exp(-1*data[i,2] * (wl2.1-wl0) + data[i,4])) / (wl2.2-wl2.1))
  output[i,1] = temp1/temp2
  }
  
  return(output)
}


#DOC must be in mg/L, with sample names as row names and one column with the values
#If DOC is in µM, change "unit" to "uM"
#Column one on CDOM is wavelength (nm) and column two is absorption coefficient
#wl is set at 254 for SUVA254 but can be change to obtain any desired wavelength
#skip is the number of lines to skip before getting the header in CDOM files

SUVA <- function(FileSelect = F,DOC, wl = 254, unit = "mg/L", skip = 1, name="SUVAMatches")
{
  if(unit == "uM") DOC = DOC * 12 / 1000
  setwd("./data/CDOM")
  if(!FileSelect) file.dir = list.files()
  if(FileSelect) file.dir = choose.files()
  
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
    Abs = read.table(file.dir[i], skip = skip, header = skip + 1, sep=",")
    a254 = Abs[,2][Abs[,1] == wl]
    SUVA254[i,1] = a254 / DOC[i]
  }
  
  #Return to main folder
  setwd("..")
  setwd("..")
  #Name column and rows
  rownames(SUVA254) = rownames(DOC)
  colnames(SUVA254) = "SUVA"
  #Return the values
  return(SUVA254)
}

