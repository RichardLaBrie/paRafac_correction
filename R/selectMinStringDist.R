#This function to match names is used by two other functions in the package
#It automatically mathces the name from a source name to another
#For example, it will match the names of DOC samples to the CDOM filenames

#name is the .csv filename that will be created to verify if the matches is done correctly

selectMinStringDist = function(target, source, name)
{
  dist = stringdist::stringdist(target, source, method="lcs")
  index = which.min(dist)
  min = dist[index]
  write.table(t(c(target, source[index] ,min)), paste(name,".csv",sep=""), append=T, sep = ",", col.names = F)
  return(c(index))
}