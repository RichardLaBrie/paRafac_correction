#'
#'@title Subtract and plot fluorescence Excitation-Emission Matrix (EEM)
#'
#'@description
#'A simple function to visualize EEMs obtained using a Varian Cary Eclipse fluorometer in csv format
#'(could evolved to include other instrument). The user is prompted to select one or many files
#'
#'
#'@param path Full path of the working directory (can be called using getwd())
#'@param zlim is the limits of the fluoresence scale (z axis) in raw units. Default is c(0, 10) for clear oceanic waters.
#'@param SUBTRACT.BLANK is a logical parameter to indicate if a blank EMM is to be subtract from the sample EEM.
#'Default is TRUE and the user is prompted to first select the blank file.
#'@param PLOT.BLANK is a logical parameter indicating whether the blank EEMs is ploted or not. Default is TRUE.
#'@param PLOT.RAMAN is a logical parameter indicating
#'whether or not the raman peak is ploted. Default is TRUE.
#'@param excitation is a vector of three variables of the scanning setup (min, max, interval).
#'Default is c(220, 450, 5)
#'@param emission is a vector of three variables of the scanning setup (min, max, interval).
#'Default is c(230, 600, 2)
#'@param EMCOL is a logical parameter indicating whether or not the emission are
#'stored as column in the csv file. Default is FALSE.
#'@param samplepercsv is a parameter which indicates the number of sample in the csv file coming from the fluorometer.
#'@param RU is a logical parameter to transform fluorescence intensities into Raman Unit at Ex = 350 nm.
#'Default is TRUE
#'
#'
#'
#'@details
#'
#'@return Returns the EEM from the sample
#'
#'@seealso \code{\link{read.EEM}}
#'
#'@author
#'   Simon Bélanger & Richard LaBrie
#'
#'@examples
#'
#'plot.EEM.go(getwd(), zlim = c(0, 20), SUBTRACT.BLANK = FALSE)
#'
#'@export


plot.EEM.go <- function(path, zlim = c(0, 10), 
            SUBTRACT.BLANK = TRUE, PLOT.RAMAN = TRUE, PLOT.BLANK = TRUE, 
            excitation = c(220, 450, 5), emission = c(230, 600, 2), EMCOL = FALSE, samplepercsv = 1, RU = T)
{
	setwd(path)
	if (RU)
	{
		file.nano = choose.files(caption = "Select Raman file")
		Raman = read.EEM.Richard(file.nano, excitation, emission, EMCOL)
		RamanInt = plot.integrate.RAMAN.go(Raman, maxF, graph = F)
	}
	
	if (SUBTRACT.BLANK)
	{
		if (.Platform$OS.type == "unix")
		{
			print("Select Nano pure water file")
			file.nano  = file.choose()
			print("Select sample file(s)")
			file.sample = rchoose.files(path)
		}
		else 
		{
		 if(RU)
		 {
		  file.sample = choose.files(caption = "Select sample file(s)")
		 }
		 else
		 {
		  file.nano = choose.files(caption = "Select Nano pure water file")
			 file.sample = choose.files(caption = "Select sample file(s)")
		 }
			
		}
	 if(RU)
	 {
	  NANO = Raman
	 }
	 else
	 {
	  NANO = read.EEM.Richard(file.nano, excitation, emission, EMCOL)
		 RamanInt = plot.integrate.RAMAN.go(NANO, maxF)
	 }
	
		if (length(file.sample)>1) 
		{
			for (i in 1: length(file.sample)) 
			{
				print(file.sample[i])
				EEM = read.EEM.Richard(file.sample[i], excitation, emission, EMCOL, samplepercsv)
				EEM = subtract.blank.plot.EEMs(NANO, EEM, zlim, PLOT.BLANK, RamanInt, samplepercsv, RU) # here we pass the file names instead of the eems
			}
		}	
		else
		{
			EEM = read.EEM.Richard(file.sample, excitation, emission, EMCOL, samplepercsv)
			EEM = subtract.blank.plot.EEMs(NANO, EEM, zlim, PLOT.BLANK, RamanInt, samplepercsv, RU)
		}
		if (PLOT.RAMAN)
		{
			RAMAN = plot.integrate.RAMAN.go(NANO, maxF)
		}

	}
	else
	{
	if (.Platform$OS.type == "unix")
		{
			print("Select sample file(s)")
			file.sample = rchoose.files(path)
		}
		else 
		{
			file.sample = choose.files(caption = "Select sample file(s)")
		}
		if (file.sample == 1)
		{
			if (samplepercsv == 1)
			{
				EEM = read.EEM.Richard(file.sample, excitation, emission, EMCOL, samplepercsv)
				par(mar = c(8, 8, 5, 4))
				if(RU)
				{
					zlim = zlim / RamanInt
					EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
					filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
					main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
				}
				else
				{
					filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
					main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
				}
			}
			else
			{
				if (samplepercsv == 2)
				{
					EEM = read.EEM.Richard(file.sample, excitation, emission, EMCOL, samplepercsv)
					par(mar = c(8, 8, 5, 4))
					if(RU)
					{
						zlim = zlim / RamanInt
						EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
						EEM$EEM.list$eem2 = EEM$EEM.list$eem2 / RamanInt
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
					}
					else
					{
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
					}
				}
				else
				{
					if (samplepercsv == 3)
					{
						EEM = read.EEM.Richard(file.sample, excitation, emission, EMCOL, samplepercsv)
						par(mar = c(8, 8, 5, 4))
						if(RU)
						{
							zlim = zlim / RamanInt
							EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
							EEM$EEM.list$eem2 = EEM$EEM.list$eem2 / RamanInt
							EEM$EEM.list$eem3 = EEM$EEM.list$eem3 / RamanInt
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
						}
						else
						{
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
						}
					}
					else
					{
						EEM = read.EEM.Richard(file.sample, excitation, emission, EMCOL, samplepercsv)
						par(mar = c(8, 8, 5, 4))
						if(RU)
						{
							zlim = zlim / RamanInt
							EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
							EEM$EEM.list$eem2 = EEM$EEM.list$eem2 / RamanInt
							EEM$EEM.list$eem3 = EEM$EEM.list$eem3 / RamanInt
							EEM$EEM.list$eem4 = EEM$EEM.list$eem4 / RamanInt
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem4 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name4, zlim = zlim, nlevels = 50)
						}
						else
						{
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem4 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name4, zlim = zlim, nlevels = 50)
						}
					}
				}
			}
		}
		else
		{
			if (samplepercsv == 1)
			{
				for(i in 1: length(file.sample))
				{
					EEM = read.EEM.Richard(file.sample[i], excitation, emission, EMCOL, samplepercsv)
					par(mar = c(8, 8, 5, 4))
					if(RU)
					{
						if(i == 1) { RUlim = zlim / RamanInt }
						EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
						main = EEM$EEM.name$name, zlim = RUlim, nlevels = 50)
					}
					else
					{
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
						main = EEM$EEM.name$name, zlim = zlim, nlevels = 50)
					}
				}
			}
			else
			{
				if (samplepercsv == 2)
				{
					for(i in 1:length(file.sample))
					{
						EEM = read.EEM.Richard(file.sample[i], excitation, emission, EMCOL, samplepercsv)
						par(mar = c(8, 8, 5, 4))
						if(RU)
						{
							if(i == 1) { RUlim = zlim / RamanInt }
							EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
							EEM$EEM.list$eem2 = EEM$EEM.list$eem2 / RamanInt
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name1, zlim = RUlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name2, zlim = RUlim, nlevels = 50)
						}
						else
						{
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
							filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
								main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
						}
					}
				}
				else
				{
					if (samplepercsv == 3)
					{
						for(i in 1:length(file.sample))
						{
							EEM = read.EEM.Richard(file.sample[i], excitation, emission, EMCOL, samplepercsv)
							par(mar = c(8, 8, 5, 4))
							if(RU)
							{
								if(i == 1) { RUlim = zlim / RamanInt }
								EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
								EEM$EEM.list$eem2 = EEM$EEM.list$eem2 / RamanInt
								EEM$EEM.list$eem3 = EEM$EEM.list$eem3 / RamanInt
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name1, zlim = RUlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name2, zlim = RUlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name3, zlim = RUlim, nlevels = 50)
							}
							else
							{
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
							}
						}
					}
					else
					{
						for (i in 1:length(file.sample))
						{
							EEM = read.EEM.Richard(file.sample[i], excitation, emission, EMCOL, samplepercsv)
							par(mar = c(8, 8, 5, 4))
							if(RU)
							{
								if(i == 1)	{ RUlim = zlim / RamanInt }
								EEM$EEM.list$eem = EEM$EEM.list$eem / RamanInt
								EEM$EEM.list$eem2 = EEM$EEM.list$eem2 / RamanInt
								EEM$EEM.list$eem3 = EEM$EEM.list$eem3 / RamanInt
								EEM$EEM.list$eem4 = EEM$EEM.list$eem4 / RamanInt
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name1, zlim = RUlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name2, zlim = RUlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name3, zlim = RUlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem4 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name4, zlim = RUlim, nlevels = 50)
							}
							else
							{
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
								filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem4 , color.palette = myPalette, xlab = "Excitation (nm)", ylab = "Emission (nm)", 
									main = EEM$EEM.name$name4, zlim = zlim, nlevels = 50)
							}
						}
					}
				}
			}
		return(EEM)
		}
	}
}