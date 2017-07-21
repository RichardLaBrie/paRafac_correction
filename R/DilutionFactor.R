#'@title Multiply EEMs by their dilution factor

#'@description This function recognize dilution factor in CDOM file name if written properly
#'The output returns the data cube corrected for dilution or a vector with dilution factor
#'Name example: SampleName_15x.csv
#'@param cube is the output of PARAFAC.cube.design
#'@param vector.out is a logical parameter to return dilution factor vector. Default is FALSE, returning corrected cube

#'@export

DilutionFactor <- function(cube, vector.out = F)
{
  #Reads csv file with CDOM names
  dilution.temp = read.csv("./data/verifyMatches.csv",header=T)[,3]
  
  #Select the dilution factor
  dil.vec = as.numeric(gsub(pattern = ".*_|x.*", replacement = "", dilution.temp)) 
  #Replace NA with 1 if not diluted
  dil.vec[which(is.na(dil.vec))]=1
  
  #Returns dilution vector
  if(vector.out) return(dil.vec)
  
  #Apply dilution to EEMs
  for(i in 1:length(dil.vec))
  {
    cube[[1]][,,i] = cube[[1]][,,i] * dil.vec[i]
  }
  
  #Store dilution vector in list
  cube[[6]] = dil.vec
  
  #Returns list
  return(cube)
}