subtract.blank.plot.EEMs <- function(NANO, EEM, zlim = c(0, 10), PLOT.BLANK = TRUE, RamanInt = 1, samplepercsv = 1, RU = TRUE)
{

 #for (i in 1:length(EEM$wlex)) EEM$EEM.list$eem[i, EEM$wlem < (EEM$wlex[i]+5)] = NA
 #for (i in 1:length(NANO$wlex)) NANO$EEM.list$eem[i, NANO$wlem < (NANO$wlex[i]+5)] = NA


 par(mar = c(8, 8, 5, 4))
 if (PLOT.BLANK)
	{
		filled.contour(NANO$wlex, NANO$wlem, NANO$EEM.list$eem, color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", 
					main = NANO$EEM.name$name1, zlim = zlim, nlevels = 50)
		if(samplepercsv == 1)
		{
			if(RU)
			{
				RUEEM <- EEM$EEM.list$eem / RamanInt
				RUzlim = zlim / RamanInt
				filled.contour(EEM$wlex, EEM$wlem, RUEEM, color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", 
					main = EEM$EEM.name$name1, zlim = RUzlim, nlevels = 50)
			}
			else
			{
				filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem, color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", 
					main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
			}
		}
		else
		{
			if(samplepercsv == 2)
			{
				if(RU)
				{
					RUEEM <- EEM$EEM.list$eem / RamanInt
					RUEEM2 <- EEM$EEM.list$eem2 / RamanInt
					RUzlim = zlim / RamanInt
					filled.contour(EEM$wlex, EEM$wlem, RUEEM, color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", 
						main = EEM$EEM.name$name1, zlim = RUzlim, nlevels = 50)
					filled.contour(EEM$wlex, EEM$wlem, RUEEM2, color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", 
						main = EEM$EEM.name$name2, zlim = RUzlim, nlevels = 50)
				}
				else
				{
					filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem, color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", 
						main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
					filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2, color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", 
						main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
				}
			}
			else
			{
				if(samplepercsv == 3)
				{
					if(RU)
					{
						RUEEM <- EEM$EEM.list$eem / RamanInt
						RUEEM2 <- EEM$EEM.list$eem2 / RamanInt
						RUEEM3 <- EEM$EEM.list$eem3 / RamanInt
						RUzlim = zlim / RamanInt
						filled.contour(EEM$wlex, EEM$wlem, RUEEM, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name1, zlim = RUzlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, RUEEM2, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name2, zlim = RUzlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, RUEEM3, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name3, zlim = RUzlim, nlevels = 50)
					}
					else
					{
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
					}
				}
				else
				{
					if(RU)
					{
						RUEEM <- EEM$EEM.list$eem / RamanInt
						RUEEM2 <- EEM$EEM.list$eem2 / RamanInt
						RUEEM3 <- EEM$EEM.list$eem3 / RamanInt
						RUEEM4 <- EEM$EEM.list$eem4 / RamanInt
						RUzlim = zlim / RamanInt
						filled.contour(EEM$wlex, EEM$wlem, RUEEM, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name1, zlim = RUzlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, RUEEM2, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name2, zlim = RUzlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, RUEEM3, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name3, zlim = RUzlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, RUEEM4, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name4, zlim = RUzlim, nlevels = 50)
					}
					else
					{
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name1, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem2, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name2, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem3, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name3, zlim = zlim, nlevels = 50)
						filled.contour(EEM$wlex, EEM$wlem, EEM$EEM.list$eem4, color.palette = myPalette, 
							xlab = "Excitation (nm)", ylab = "Emission (nm)", 
							main = EEM$EEM.name$name4, zlim = zlim, nlevels = 50)
					}
				}
			}
		}
	}

	if(samplepercsv == 1)
	{
		if(RU)
		{
			CorrectedEEM = (EEM$EEM.list$eem - NANO$EEM.list$eem) / RamanInt
			zlim = zlim / RamanInt
			filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
         xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
         main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)

			png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
			filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
         xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
         main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
			dev.off()

			return(list(eem.cor = CorrectedEEM, wlem = EEM$wlem, wlex = EEM$wlex))
		}
		else
		{
			CorrectedEEM = EEM$EEM.list$eem - NANO$EEM.list$eem

			filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
         xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
         main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)

			png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
			filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
         xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
         main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
			dev.off()

			return(list(eem.cor = CorrectedEEM, wlem = EEM$wlem, wlex = EEM$wlex))
		}
	}
	else
	{
		if(samplepercsv == 2)
		{
			if(RU)
			{
				CorrectedEEM = (EEM$EEM.list$eem - NANO$EEM.list$eem) / RamanInt
				CorrectedEEM2 = (EEM$EEM.list$eem2 - NANO$EEM.list$eem) / RamanInt
				zlim = zlim / RamanInt
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)

				png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
				png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
				dev.off()

				return(list(eem.cor = CorrectedEEM, eem.cor2 = CorrectedEEM2, wlem = EEM$wlem, wlex = EEM$wlex))
			}
			else
			{
				CorrectedEEM = EEM$EEM.list$eem - NANO$EEM.list$eem
				CorrectedEEM2 = EEM$EEM.list$eem2 - NANO$EEM.list$eem

				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
				
				png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
				dev.off()
				png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
				filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
					xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
					main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
				dev.off()

				return(list(eem.cor = CorrectedEEM, eem.cor2 = CorrectedEEM2, wlem = EEM$wlem, wlex = EEM$wlex))
			}
		}
		else
		{
			if(samplepercsv == 3)
			{
				if(RU)
				{
					CorrectedEEM = (EEM$EEM.list$eem - NANO$EEM.list$eem) / RamanInt
					CorrectedEEM2 = (EEM$EEM.list$eem2 - NANO$EEM.list$eem) / RamanInt
					CorrectedEEM3 = (EEM$EEM.list$eem3 - NANO$EEM.list$eem) / RamanInt
					zlim = zlim / RamanInt
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
						
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$filename1), zlim = zlim)
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$filename1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$filename1), zlim = zlim)
					dev.off()

					return(list(eem.cor = CorrectedEEM, eem.cor2 = CorrectedEEM2, eem.cor3 = CorrectedEEM3, wlem = EEM$wlem, wlex = EEM$wlex))
				}
				else
				{
					CorrectedEEM = EEM$EEM.list$eem - NANO$EEM.list$eem
					CorrectedEEM2 = EEM$EEM.list$eem2 - NANO$EEM.list$eem
					CorrectedEEM3 = EEM$EEM.list$eem3 - NANO$EEM.list$eem

					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
					
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()

					return(list(eem.cor = CorrectedEEM, eem.cor2 = CorrectedEEM2, eem.cor3 = CorrectedEEM3, wlem = EEM$wlem, wlex = EEM$wlex))
				}
			}
			else
			{
				if(RU)
				{
					CorrectedEEM = (EEM$EEM.list$eem - NANO$EEM.list$eem) / RamanInt
					CorrectedEEM2 = (EEM$EEM.list$eem2 - NANO$EEM.list$eem) / RamanInt
					CorrectedEEM3 = (EEM$EEM.list$eem3 - NANO$EEM.list$eem) / RamanInt
					CorrectedEEM4 = (EEM$EEM.list$eem4 - NANO$EEM.list$eem) / RamanInt
					zlim = zlim / RamanInt
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$ame1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM4 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name4, "minus", NANO$EEM.name$name1), zlim = zlim)
						
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
					png(paste(EEM$EEM.name$name, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM4 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name4, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					
					return(list(eem.cor = CorrectedEEM, eem.cor2 = CorrectedEEM2, eem.cor3 = CorrectedEEM3, wlem = EEM$wlem, wlex = EEM$wlex))
				}
				else
				{
					CorrectedEEM = EEM$EEM.list$eem - NANO$EEM.list$eem
					CorrectedEEM2 = EEM$EEM.list$eem2 - NANO$EEM.list$eem
					CorrectedEEM3 = EEM$EEM.list$eem3 - NANO$EEM.list$eem
					CorrectedEEM4 = EEM$EEM.list$eem4 - NANO$EEM.list$eem
					
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM4 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name4, "minus", NANO$EEM.name$name1), zlim = zlim)
					
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name1, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM2 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name2, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM3 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name3, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()
					png(paste(EEM$filename, '.png', sep = ""), units = "in", height = 6, width = 6, res = 300)
					filled.contour(EEM$wlex, EEM$wlem, CorrectedEEM4 , color.palette = myPalette, 
						xlab = "Excitation (nm)", ylab = "Emission (nm)", nlevels = 50, 
						main = paste(EEM$EEM.name$name4, "minus", NANO$EEM.name$name1), zlim = zlim)
					dev.off()

					return(list(eem.cor = CorrectedEEM, eem.cor2 = CorrectedEEM2, eem.cor3 = CorrectedEEM3, 
						eem.cor4 = CorrectedEEM4, wlem = EEM$wlem, wlex = EEM$wlex))
				}
			}
		}
	}
}