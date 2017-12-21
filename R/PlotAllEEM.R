#'@title This function plot all corrected EEMs
#'
#'@description This function uses the output of PARAFAC.cube.design to plot all corrected EEMss at once
#'The scale bar is the same among all samples. It also generates a pdf file containing all plots

#'@param cube is the ouput of PARAFAC.cube.design
#'@param zlim is the intensity to plot the EEMs. Negative and values outside of scale bar will be white
#'@param ExportPDF is a logical paramater to export plots in a pdf file. Default is TRUE

#'@export

PlotAllEEM <- function(cube, zlim = c(0,1), ExportPDF = T)
{
	setwd(getwd())
  par(mar = c(5, 4, 4, 3))
	if(ExportPDF)
	{
	  pdf("EEM.pdf")
	}
	  for(i in 1:cube[[5]])
	{
		filled.contour(cube[[3]], cube[[4]], unlist(cube[[1]])[,,i], color.palette = myPalette,
		xlab = "Excitation (nm)", ylab = "Emission (nm)",
		main = cube[[2]][i], zlim = zlim, nlevels = 50)
	}
	if(ExportPDF)
	{
    dev.off()
	}
 }