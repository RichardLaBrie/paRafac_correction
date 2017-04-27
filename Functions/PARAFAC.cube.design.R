#'@title Combine EEMs produce by the Cary Eclipse fluorometer into a cube
#'ready for PARAFAC analysis in Matlab
#'
#'@description  This function reads any number of EEMs and produce a cube of EEMs.
#'It enables the standardization into Raman Unit and instrument's corrections.
#'
#'@path Full path of the working directory (can be called using getwd())
#'@param excitation is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(220,450,5)
#'@param emission is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(230,600,2)
#'@param  EMCOL  is a logical parameter indicating whether or not the emission are
#'stored as column in the csv file. Default is FALSE.
#'@param 
#'@param samplepercsv is a parameter which indicates the number of sample in the csv file coming from the fluorometer.
#'@param RU is a logical parameter to transform fluorescence intensities into Raman Unit at Ex = 350 nm.
#'Default is TRUE.
#'@param EmEx.cor is a logical parameter to correct EEMs for emission and excitation corrections.
#'Default is True. Emission and excitation file must be numerics only stored in csv file.
#'
#'
#'@export
#'
#'



PARAFAC.cube.design = function(path = getwd(), excitation = c(220,450,5), emission = c(230, 600, 2), EMCOL = F, Subtract.Blank = T, RU = T, rm.corner=T, EmEx.cor = T, Inner=T, pathlength = 1, split="_")
{
  samplepercsv = 4
  wlex <- seq(excitation[1], excitation[2], excitation[3])
  wlem <- seq(emission[1], emission[2], emission[3])
  nex <- length(wlex)
  nem  <- length(wlem)

	setwd(".\\data")
	
	file.dir = list.files()
	nano.temp = grep("nano", file.dir)
	cdom.temp = grep("CDOM",file.dir)
	file.dir = file.dir[-nano.temp]
	file.dir = file.dir[-cdom.temp]
	file.list = list()
	  
	for(i in 1:length(file.dir))
	{
	  file.list[[i]] = paste(file.dir[i],"/",list.files(file.dir[i]),sep="")
	}
  file.list = unlist(file.list)
  csv.count <- str_count(file.list,"_") + 1
	  
  file.sample = file.list[csv.count==1]
  file.sample2 = file.list[csv.count==2]
  file.sample3 = file.list[csv.count==3]
  file.sample4 = file.list[csv.count==4]
  
  
	#Reading the sample files and creating a list of EEMs
	counter = 1
	data.list <- list()
	filename <- list()
	index = 0
	list.length = 0
	while(counter <= samplepercsv)
	{
		if(counter == 1)
		{
			file.data <- file.sample
		}
		
		if(counter == 2)
		{
			file.data <- file.sample2
		}
		
		if(counter == 3)
		{
			file.data <- file.sample3
		}
		
		if(counter == 4)
		{
			file.data <- file.sample4
		}
				
		if(length(file.data) > 1)
		{
			for (i in 1:length(file.data))
			{
					EEM = read.EEM.Richard(file.data[i],excitation,emission, EMCOL, counter, split=split)
					data.list[[i + index]] <- EEM$EEM.list
					filename[[i + index]] <- unlist(EEM$EEM.name)
			}
			index = index + length(file.data)
			list.length = list.length + length(file.data) * counter
		}
		else
		{
			if(length(file.data)==1)
			{
				EEM = read.EEM.Richard(file.data,excitation,emission, EMCOL, counter, split=split)
				data.list[[index + 1]] = EEM$EEM.list
				filename[[index + 1]] = unlist(EEM$EEM.name)
				index = index + 1
				list.length = list.length + counter
			}
		}
		counter = counter + 1
	}
	cube <- array(unlist(data.list),dim=c(nex,nem,list.length))
	if(Subtract.Blank)
	{
	  Raman = NanoMean(path, excitation, emission, EMCOL,RU=T, split=split)
	  for(k in 1:length(cube[1,1,]))
	  {
	    cube[,,k] <- cube[,,k]- Raman[[1]]$eem[,,1]
	  }
	}
	if(Inner)
	{
	  cube = InnerFilter(path, cube, excitation, emission, pathlength,filename)
	}

	if(rm.corner)
	for(k in 1:length(cube[1,1,]))
	{
	  for(i in 1:length(cube[,1,1]))
	  {
	    cube[i,wlem<=(wlex[i]-10),k]=0 #Put all data below 1st order Rayleigh equal 0
	  }
	}
	
  if(RU)
  {
    if(Subtract.Blank == F)
    {
        Raman = NanoMean(path, excitation, emission, EMCOL, split=split,RU=T)
    }
    
    RAMANInt = plot.integrate.RAMAN(Raman, maxF, graph=F)
    cube.RU=cube
    for(k in 1:length(cube[1,1,]))
    {
      cube.RU[,,k] <- cube[,,k]/RAMANInt
    }
    
    if(EmEx.cor)
    {
      file.Em = read.csv("../Emcorr_220 to 600.csv")
      file.Ex = read.csv("../Excorr.csv")
      Ex.cor = as.numeric(na.omit(file.Ex[match(round(file.Ex[,1]),wlex),2]))
      Em.cor = t(as.numeric(na.omit(file.Em[match(round(file.Em[,1]),wlem),2])))
      Cor.mat = Ex.cor %*% Em.cor
      cube.RU.EmEx = cube.RU
      for(k in 1:length(cube.RU[1,1,]))
      {
        cube.RU.EmEx[,,k] = cube.RU[,,k] * Cor.mat
      setwd("..")
      return(list(cube.RU.EmEx,filename,wlex,wlem,list.length))
      }
    setwd("..")  
    return(list(cube.RU,filename,wlex,wlem,list.length))
    }
    if(EmEx.cor)
    {
      file.Em = read.csv("../Emcorr_220 to 600.csv")
      file.Ex = read.csv("../Excorr.csv")
      Ex.cor = as.numeric(na.omit(file.Ex[match(round(file.Ex[,1]),wlex),2]))
      Em.cor = t(as.numeric(na.omit(file.Em[match(round(file.Em[,1]),wlem),2])))
      Cor.mat = Ex.cor %*% Em.cor
      cube.EmEx = cube
      for(k in 1:length(cube[1,1,]))
      {
        cube.EmEx[,,k] = cube[,,k] * Cor.mat
      }
    setwd("..")
    return(list(cube.EmEx,filename,wlex,wlem,list.length))
    }
  setwd("..")
	return(list(cube,filename,wlex,wlem,list.length))
  }
}

