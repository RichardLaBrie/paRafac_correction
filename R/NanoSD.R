#'@title This function calculates and plots standard deviation of nano files
#'
#'@description This function uses NanoMean to compute the standrad deviation to make sure that all nano
#' files are similare

#'@param excitation is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(220,450,5)
#'@param emission is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(230,600,2)
#'@param EMCOL is a logical parameter indicating whether or not the emission are
#'stored as column in the csv file. Default is FALSE.
#'@param split is the symbol used to separate each EEM when using a multicell holder
#'@param data.file is the name of the main folder where CDOM, FDOM and nano folders are
#'@param fluorometer is a paramater for the fluorometer model. Default is "Cary Eclipse".
#'Other model supported: "Shimadzu"
#'@param EEMskip is a parameter to skip lines in EEM file before data. Default is 1
#'@param sd is a logical parameter to calculate standard deviation with the NanoMean function. Default is True
#'@param zlim is the intensity to plot the EEMs. Negative and values outside of scale bar will be white

#'@export


NanoSD = function(excitation = c(220,450,5), emission = c(230, 600, 2), EMCOL = F, split="_", data.file = "data",
                    fluorometer = "Cary Eclipse", EEMskip, sd=T, zlim = c(0,1))
{
  output = NanoMean(excitation, emission, EMCOL, RU = F, split, data.file, fluorometer, EEMskip, sd = sd)
  filled.contour(output[[2]], output[[3]], output[[1]]$eem, color.palette = myPalette,
                 xlab = "Excitation (nm)", ylab = "Emission (nm)",
                 main = "Nano files standard deviation matrix", zlim = zlim, nlevels = 50)
  
}