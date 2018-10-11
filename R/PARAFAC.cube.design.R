#'@title Combine EEMs produce by the Cary Eclipse fluorometer into a cube
#'ready for PARAFAC analysis in Matlab
#'
#'@description This function reads any number of EEMs and produce a cube of EEMs.
#'It enables the standardization into Raman Unit and instrument's corrections.
#'@param data.file is the name of the main folder where CDOM, FDOM and nano folders are
#'@param excitation is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(220,450,5)
#'@param emission is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(230,600,2)
#'@param EMCOL is a logical parameter indicating whether or not the emission are
#'stored as column in the csv file. Default is FALSE.
#'@param RU is a logical parameter to transform fluorescence intensities into Raman Unit at Ex = 350 nm.
#'Default is TRUE.
#'@param rm.corner is a logical parameter to set the bottom right corner values to 0
#'@param EmEx.cor is a logical parameter to correct EEMs for emission and excitation corrections.
#'Default is True. Emission and excitation file must be numerics only stored in csv file.
#'@param Inner is a logical parameter to apply inner filter effect correction. Default is True
#'@param pathlength is a real number indicating the length of the optical path in the spectrophotometer in cm.
#'Default is 1
#'@param split is the symbol used to separate each EEM when using a multicell holder
#'@param skip is a parameter to determine how many lines will be skiped before the header in the absorbance files
#'@param dot.number is the number of "." in the name of your EEMs file. This number includes the "." in ".csv"
#'@param NonNegativity is a logical parameter to transform all negative fluorescence values into 0. Default is TRUE
#'@param fluorometer is a paramater for the fluorometer model. Default is "Cary Eclipse".
#'Other model supported: "Shimadzu"
#'@param EEMskip is a parameter to skip lines in EEM file before data. Default is 1
#'@param rm.scat is a logical parameter to remove Reyleigh and Raman scatterings.
#'@param zlim is the intensity to plot the EEMs. Negative and values outside of scale bar will be white
#'@param graph is a logical parameter to plot the Raman integration to transform data into RU. Default is false
#'@param RspectroAbs is a logical parameter for the inner filter correction. When TRUE, CDOM data have been first treated by the 
#'RspectroAbs package developed by Simon Bélanger. Default is FASLE



#'@export
PARAFAC.cube.design = function(data.file = "data", excitation = c(220,450,5), emission = c(230, 600, 2),
                               EMCOL = F, Subtract.Blank = T, RU = T, rm.corner = T, EmEx.cor = T, Inner = T,
                               pathlength = 1, split = "_", skip = 1,  dot.number = 1, NonNegativity = T,
                               fluorometer = "Cary Eclipse", EEMskip = 1, rm.scat = T, zlim = c(0,1), graph = F, RspectroAbs = F)
{
  samplepercsv = 4
  wlex = seq(excitation[1], excitation[2], excitation[3])
  wlem = seq(emission[1], emission[2], emission[3])
  nex = length(wlex)
  nem  = length(wlem)

	file.dir = list.files(paste0("./",data.file,"/FDOM"))
	#nano.temp = grep("nano", file.dir)
	#cdom.temp = grep("CDOM", file.dir)
	#file.dir = file.dir[-nano.temp]
	#file.dir = file.dir[-cdom.temp]
	#file.list = list()
	 
	#for(i in 1:length(file.dir))
	#{
	  #file.list[[i]] = paste(file.dir[i], "/", list.files(file.dir[i]), sep = "")
	#}
  #file.list = unlist(file.list)
	csv.count = stringr::str_count(file.dir, "_") + 1 #when file.dir it was file.list
	 
  file.sample = file.dir[csv.count == 1]
  file.sample2 = file.dir[csv.count == 2]
  file.sample3 = file.dir[csv.count == 3]
  file.sample4 = file.dir[csv.count == 4]
 
 
	#Reading the sample files and creating a list of EEMs
	counter = 1
	data.list = list()
	filename = list()
	index = 0
	list.length = 0
	subfolder = "/FDOM/"
	while(counter <= samplepercsv)
	{
		if(counter == 1)
		{
			file.data = file.sample
		}
		
		if(counter == 2)
		{
			file.data = file.sample2
		}
		
		if(counter == 3)
		{
			file.data = file.sample3
		}
		
		if(counter == 4)
		{
			file.data = file.sample4
		}
				
		if(length(file.data) > 1)
		{
			for (i in 1:length(file.data))
			{
					EEM = read.EEM(file.data[i], excitation, emission, EMCOL, counter, split, dot.number,
					               fluorometer, EEMskip, data.file = data.file, subfolder = subfolder)
					data.list[[i + index]] = EEM$EEM.list
					filename[[i + index]] = unlist(EEM$EEM.name)
			}
			index = index + length(file.data)
			list.length = list.length + length(file.data) * counter
		}
		else
		{
			if(length(file.data) == 1)
			{
				EEM = read.EEM(file.data, excitation, emission, EMCOL, counter, split, dot.number,
				               fluorometer, EEMskip, data.file = data.file, subfolder = subfolder)
				data.list[[index + 1]] = EEM$EEM.list
				filename[[index + 1]] = unlist(EEM$EEM.name)
				index = index + 1
				list.length = list.length + counter
			}
		}
		counter = counter + 1
	}
	cube = array(unlist(data.list), dim = c(nex, nem, list.length))
	
	if(Subtract.Blank)
	{
	 Raman = NanoMean(excitation, emission, EMCOL, RU = T, split, data.file, fluorometer, EEMskip)
	 for(k in 1:length(cube[1,1,]))
	 {
	  cube[,,k] = cube[,,k] - Raman[[1]]$eem[,,1]
	 }
	}
	if(Inner)
	{
	 cube = InnerFilter(cube, excitation, emission, pathlength, filename, skip, data.file, RspectroAbs)
	}

	if(rm.corner)
	for(k in 1:length(cube[1,1,]))
	{
	 for(i in 1:length(cube[,1,1]))
	 {
	  cube[i, wlem <= (wlex[i]-10), k] = 0 #Put all data below 1st order Rayleigh equal 0
	 }
	}
	
  if(RU)
  {
    if(Subtract.Blank == F)
    {
      Raman = NanoMean(excitation, emission, EMCOL, split = split, RU = T, data.file, fluorometer, EEMskip)
    }
  
    RAMANInt = plot.integrate.RAMAN(Raman, maxF, graph = graph)
    for(k in 1:length(cube[1,1,]))
    {
    cube[,,k] = cube[,,k] / RAMANInt
    }
  
  }
  if(EmEx.cor)
  {
   file.Em = read.csv("Emcorr.csv")
   file.Ex = read.csv("Excorr.csv")
   Ex.cor = file.Ex[which(round(file.Ex$Wavelength..nm.) %in% wlex),2]
   Em.cor = file.Em[which(round(file.Em$Wavelength..nm.) %in% wlem),2]
   Cor.mat = Ex.cor %*% t(Em.cor)
   for(k in 1:length(cube[1,1,]))
   {
    cube[,,k] = cube[,,k] * Cor.mat
   }
  }
  
	if(rm.scat)
	{
	  while(1)
	  {
	    CubeTemp = cube[,,1]
	    ScatRangeList = readline(prompt = "Enter a vector of 8 values for Raman and Reyleigh scatterings (Ex. 10,10,10,10,10,10,10,10)")
	    ScatRange = as.numeric(unlist(strsplit(ScatRangeList, ",")))
	    for(i in 1:length(CubeTemp[,1]))
	    {
	      CubeTemp[i,wlem<=(wlex[i]+ScatRange[2]) & wlem >=(wlex[i]-ScatRange[1])]=NA #Removes band of 1st order Rayleigh
	      CubeTemp[i,wlem<=(-wlex[i] / (0.00036*wlex[i]-1) +ScatRange[4]) & wlem >=(-wlex[i] / (0.00036*wlex[i]-1) - ScatRange[3])] = NA
	      CubeTemp[i,wlem<=(2*wlex[i]+ScatRange[6]) & wlem >=(2*wlex[i]-ScatRange[5])]=NA #Removes band of 2nd order Rayleigh
	      CubeTemp[i,wlem<=(-2*wlex[i] / (0.00036*wlex[i]-1) +ScatRange[8]) & wlem >=(-2*wlex[i] / (0.00036*wlex[i]-1) - ScatRange[3])] = NA
	    }
	    filled.contour(wlex, wlem, CubeTemp, color.palette = myPalette,
	                   xlab = "Excitation (nm)", ylab = "Emission (nm)",
	                   main = "Example", zlim = zlim, nlevels = 50)
	    Exit = readline(prompt = "Are you satisfified with the graph (Y/N)")
	    if(Exit == "Y") break()
	  }
	  
	  for(k in 1:length(cube[1,1,]))
	  {
	    for(i in 1:length(cube[,1,1]))
	    {
	      cube[i,wlem<=(wlex[i]+ScatRange[2]) & wlem >=(wlex[i]-ScatRange[1]),k] = NA #Removes band of 1st order Rayleigh
	      cube[i,wlem<=(-wlex[i] / (0.00036*wlex[i]-1) +ScatRange[4]) & wlem >=(-wlex[i] / (0.00036*wlex[i]-1) - ScatRange[3]),k] = NA #Removes band of 1st order Raman
	      cube[i,wlem<=(2*wlex[i]+ScatRange[6]) & wlem >=(2*wlex[i]-ScatRange[5]),k] = NA #Removes band of 2nd order Rayleigh
	      cube[i,wlem<=(-2*wlex[i] / (0.00036*wlex[i]-1) +ScatRange[4]) & wlem >=(-2*wlex[i] / (0.00036*wlex[i]-1) - ScatRange[3]),k] = NA #Removes band of 2nd order Raman
	    }
	  }
	}
	
  if(NonNegativity) cube[cube < 0] = 0
  
	return(list(cube, as.vector(unlist(filename)), wlex, wlem, list.length))
}

