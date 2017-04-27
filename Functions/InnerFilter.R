InnerFilter = function(path, cube, excitation = c(220,450,5), emission = c(230, 600, 2),pathlength=1,name.ref)
{
	wlex <- seq(excitation[1], excitation[2], excitation[3])
	wlem <- seq(emission[1], emission[2], emission[3])
  filename = unlist(name.ref)
	
  
	file.dir = list.files()
	nano.temp = grep("nano", file.dir)
	fdom.temp = grep("FDOM",file.dir)
	file.dir = file.dir[-nano.temp]
	file.dir = file.dir[-fdom.temp]
	file.list = list()
	  
	for(i in 1:length(file.dir))
	{
	  file.list[[i]] = paste(file.dir[i],"/",list.files(file.dir[i]),sep="")
	}
	
	file.data = unlist(file.list)
  file.create("..\\verifyMatches.csv")
  write.table(t(c("Target","Source","Distance")),"..\\verifyMatches.csv",append=T,sep=",",col.names = F)
  
	selectMinStringDist <- function(target,source)
	{
  	dist=stringdist(target,source,method="lcs")
	  index=which.min(dist)
	  min=dist[index]
	  #if((dist[index]-12)>0)warning(paste("Dist was",dist[index]-12,"for",target,source[index]))
	  write.table(t(c(target,source[index],min)),"..\\verifyMatches.csv",append=T,sep=",",col.names = F)
	  return(c(index))
	}

	index=sapply(filename,	selectMinStringDist,source=file.data)
  file.data=file.data[index]
	
	for(i in 1:length(file.data))
	{
		Abs = read.table(file.data[i],skip=1,header=2,sep=",")
		WV <- Abs[,1]
		Absex = sapply(wlex,function(x){return(subset(Abs[,2],WV==x))})
		Absem = sapply(wlem,function(x){return(subset(Abs[,2],WV==x))})
		Amat = outer(Absex,Absem,'+')
		Amat2 = 10^(-0.5*pathlength*Amat)
		cube[,,i] = cube[,,i]/Amat2
	}
return(cube)
}