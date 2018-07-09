#'@title Generate the spectral slope for CDOM samples
#'
#'@description This function reads any number of CDOM files and calculate their spectral slope.
#'You have to manually load DOC file for SUVA
#'Wavelengths chosen based on Helms 2008
#'Calculate spectral slope with a reference wavelength
#'The model use a nonlinear least square fitting routine
#'The equation is abs = abs0 * exp(-S(wavelength - ref.wavelength)) + K as in Markager & Vincent 2000
#'where abs is the absorption coef, abs0 is the reference absorption coef at wavelenth and ref.wavelength
#'S is the slope and K is an additional background parameter to allow baseline to shift 

#'@param FileSelect is a logical parameter to allow you which file to load. Default is FALSE, meaning that all files in /CDOM
#'will be loaded
#'@param wl0 is the reference wavelength, default is 375 as in Stedmon, Markager and Kaas 2000
#'@param FROM is the wavelenth at which you want to start calculating the exponential fit
#'@param To is the wavelenth at which you want to end calculating the exponential fit
#'@param By is the step between each wavelength
#'@param skip is a parameter to determine how many lines will be skiped before the header in the absorbance files
#'

#'@export

SpectralSlope <- function(data.file = "data", FileSelect = F, wl0 = 375, From = 275, To = 650, By = 1, skip = 1)
{
  if(!FileSelect) file.dir = list.files(paste0("./",data.file,"/CDOM"))
  if(FileSelect) file.dir = choose.files(paste0("./",data.file,"/CDOM"))
  #create sequence of desired wavelengths
  wl.x = seq(From, To, By)
  #Create the output matrix
  output = matrix(0,ncol = 4, nrow = length(file.dir))
  colnames(output) = c("Intercept", "Slope", "R2", "K")
  rownames(output) = file.dir
  
  #Compute the exponential fit for each CDOM file
  for(i in 1:length(file.dir))
  {
    Abs = read.table(paste0("./",data.file,"/CDOM/",file.dir[i]), skip = skip, header = skip + 1, sep=",")
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
  
  #Return the result
  return(output)
}

