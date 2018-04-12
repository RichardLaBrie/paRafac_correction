#'@title Generic functions for commonly used FDOM indices
#'
#'@description These functions are designed to work with the output of PARAFAC.cube.design and calculate basic FDOM indices.
#'@references Fluorescence index (FI) from McKnight et al (2001) and Cory and McKnight (2005)
#'@references Biological index (BIX) from Wilson and Xenopoulos (2009)
#'@references Humification index (HIX) from Ohno et al (2002)

#'@param cube is the output of PARAFAC.cube.design()
#'@param cor is a logical argument for instruments corrections applied to EEMs. Default is TRUE
#'@param rel is a logical argument to scale the values of HIX

#'@export

#These functions are designed to work with the output of PARAFAC.cube.design()
#They can't be used directly with fluorescence scan unless they are stacked in a array (Ex, Em, Samples)

#Descriptions of the indices may be found in Gabor et al. Fluorescence Indices and Their Interpretation
#Or in Gao et al. Spectral characteristics of DOM in various soils throughout China




FDOMIndices <- function(cube, cor, rel)
{
  output = matrix(0, nrow = cube[[5]], ncol=3)
  rownames(output) = cube[[2]]
  colnames(output) = c("FI","BIX","HIX")
  output[,1] = FI(cube, cor)
  output[,2] = BIX(cube)
  output[,3] = HIX(cube, rel)
  return(output)
}
  
  
FI <- function(cube, cor = T)
{
  if(!is.list(cube)) return(print("You need the complete list of PARAFAC.cube.design output"))
  
  #Define wavelength to be used. Emission changes if the data are corrected for the instrument
  if(cor)
  {
    em1 = 470
    em2 = 520
  }
  else
  {
    em1 = 450
    em2 = 500
  }
    ex = 370
  
  output = matrix(0, nrow = cube[[5]])
  rownames(output) = cube[[2]]
  colnames(output) = "FI"
  for(k in 1:cube[[5]])
  {
    Fluo.em1 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em1), k]
    Fluo.em2 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em2), k]
    output[k,1] = Fluo.em1 / Fluo.em2
  }
  
  return(output)
}


BIX <- function(cube)
{
  if(!is.list(cube)) return(print("You need the complete list of PARAFAC.cube.design output"))
  
  em1 = 380
  em2 = 420
  em3 = 436
  ex = 310
  
  output = matrix(0, nrow = cube[[5]])
  rownames(output) = cube[[2]]
  colnames(output) = "BIX"
  for(k in 1:cube[[5]])
  {
    Fluo.em1 = cube[[1]][which(cube[[3]]==ex), which(cube[[4]]==em1), k]
    Fluo.em2 = cube[[1]][which(cube[[3]]==ex), c(which(cube[[4]]==em2):which(cube[[4]] == em3)), k]
    Fluo.em2 = c(Fluo.em2, mean(c(Fluo.em2[length(Fluo.em2) - 1], Fluo.em2[length(Fluo.em2)])))
    output[k,1] = Fluo.em1 / max(Fluo.em2)
  }
  
  return(output)
}



HIX <- function(cube, rel = FALSE)
{
  if(!is.list(cube)) return(print("You need the complete list of PARAFAC.cube.design output"))
  
  output = matrix(0, nrow = cube[[5]])
  rownames(output) = cube[[2]]
  colnames(output) = "HIX"
  
  #Search if excitation wavelengths include 254nm
  ex = ifelse(is.na(match(254,cube[[3]])), 255, 254)
  #Prepare the matrix to interpolate at 254nm, internal function
  inter = function(data)
  {
    data[1,] = 0.2 * data[1,]
    data[2,] = 0.8 * data[2,]
    return(data)
  }
  
  for(k in 1:cube[[5]])
  {
    
    #Extract data from de matrix at 250 and 255 nm excitaion and 434-480 emission
    if(cube[[4]][2] - cube[[4]][1] == 2)
    {
      grid434_480 = cube[[1]][c(which(cube[[3]]==250):which(cube[[3]]==ex)),
                     c(which(cube[[4]]==434):which(cube[[4]]==480)),k] 
      
      
      #Estimate values at 254 nm
      grid434_480 = if(ex == 255) { colSums(inter(grid434_480))} else{ grid434_480[2,]}
      
      grid435_479=NA
      #Interpolate values for odd emissions' nm
      for(x in 1:length(grid434_480)-1)
      {
        grid435_479[x] = mean(c(grid434_480[x],grid434_480[x+1]))
      }
      #Sum all data, except for emission = 434nm
      em435_480 = sum(c(grid435_479, grid434_480[-1]))
    
      #Extract data from de matrix at 250 and 255 nm excitaion and 300-346 emission
      grid300_346 = cube[[1]][c(which(cube[[3]]==250):which(cube[[3]]==ex)),
                            c(which(cube[[4]]==300):which(cube[[4]]==346)),k]
      
      #Estimate values at 254 nm
      grid300_346 = if(ex == 255) { colSums(inter(grid300_346))} else{ grid300_346[2,]}
     
      grid301_345=NA
      
      #Interpolate values for odd emissions' nm
      for(x in 1:length(grid300_346)-1)
      {
        grid301_345[x] = mean(c(grid300_346[x],grid300_346[x+1]))
      }  
      
      #Sum all data, except for emission = 434nm
      em300_345 = sum(c(grid301_345, grid300_346[-length(grid300_346)]))
    } else
      
      {
        grid435_480 = cube[[1]][c(which(cube[[3]]==250):which(cube[[3]]==ex)),
                              c(which(cube[[4]]==435):which(cube[[4]]==480)),k]
        
        #Estimate values at 254 nm
        grid435_480 = if(ex == 255) { colSums(inter(grid435_480))} else{ grid435_480[2,]}

        #Sum all values
        em435_480 = sum(grid435_480)
        
        
        grid300_345 = cube[[1]][c(which(cube[[3]]==250):which(cube[[3]]==ex)),
                                c(which(cube[[4]]==300):which(cube[[4]]==345)),k]
        
        #Estimate values at 254 nm
        grid300_345 = if(ex == 255) { colSums(inter(grid300_345))} else{ grid300_345[2,]}

        #Sum all values
        em300_345 = sum(grid300_345)
    }
      

    
    #Compute HIX
    output[k,1] <- if(rel) {em435_480 / (em435_480 + em300_345)} else{em435_480 / em300_345}
  }
 
  return(output)
}






