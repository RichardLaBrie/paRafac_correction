InnerFilter = function(cube, excitation = c(220,450,5), emission = c(230, 600, 2), pathlength=1, name.ref, skip)
{
	wlex = seq(excitation[1], excitation[2], excitation[3])
	wlem = seq(emission[1], emission[2], emission[3])
  filename = unlist(name.ref)
	
  setwd("./CDOM")
	file.dir = list.files()
	#nano.temp = grep("nano", file.dir)
	#fdom.temp = grep("FDOM",file.dir)
	#file.dir = file.dir[-nano.temp]
	#file.dir = file.dir[-fdom.temp]
	#file.list = list()
	  
	#for(i in 1:length(file.dir))
	#{
	  #file.list[[i]] = paste(file.dir[i],"/",list.files(file.dir[i]),sep="")
	#}
	
	#file.data = unlist(file.list)
  file.create("..\\verifyMatches.csv")
  write.table(t(c("Target", "Source", "Distance")), "..\\verifyMatches.csv", append=T, sep=",", col.names = F)
  
	index = sapply(filename, selectMinStringDist, source = file.dir, name = "verifyMatches") #Was source = file.data
  #file.data = file.data[index]
	file.dir = file.dir[index]
	low = F
	high = F

	for(i in 1:length(file.dir)) #when file.dir, it was file.data
	{
		Abs = read.table(file.dir[i], skip = skip, header = skip + 1, sep=",")
		if((min(wlex) | min(wlem)) <= min(Abs[,1])) low = T
		if((max(wlex) | max(wlem)) >= max(Abs[,1])) high = T
		if(low | high)
		{
			if(low) lowfrom = min(min(wlex), min(wlem))
			if(low) lowto = min(Abs[,1]) - 1
			if(high) highfrom = max(Abs[,1]) + 1
			if(high) highto = max(max(wlem), max(wlex))
			Abs = CDOMFileCorrection(CDOM = Abs, low = low, high = high, lowfrom, lowto, highfrom, highto)
		}
		WV = Abs[,1]
		Absex = sapply(wlex, function(x){return(subset(Abs[,2], WV == x))})
		Absem = sapply(wlem, function(x){return(subset(Abs[,2], WV == x))})
		Amat = outer(Absex,Absem,'+')
		Amat2 = 10^(-0.5 * pathlength * Amat)
		cube[,,i] = cube[,,i] / Amat2
	}
	setwd("..")
return(cube)
}
