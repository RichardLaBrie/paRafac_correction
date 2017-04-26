setwd("..")
source('Analyse & script R/myPalette.R')
source('Analyse & script R/NanoMean.R')
source('Analyse & script R/InnerFilter.R')
source('Analyse & script R/PARAFAC.cube.design.R')
source('Analyse & script R/plot.eem.go.richard.R')
source('Analyse & script R/plot.integrate.RAMAN.R')
source('Analyse & script R/read.EEM.Richard.R')
source('Analyse & script R/subtract.blank.plot.EEMs.Richard.R')
source('Analyse & script R/plot.integrate.RAMAN.go.R')
require(stringr)
require(stringdist)

 cube=PARAFAC.cube.design()

 pdf("EEM.pdf")
 for(i in 1:length(cube[[1]][1,1,])){filled.contour(wlex, wlem, unlist(cube[[1]])[,,i],color.palette=myPalette,
                                                    xlab="Excitation (nm)", ylab="Emission (nm)",
                                                    main=unlist(cube[[2]])[i], zlim = zlim, nlevels=50)}
 dev.off()