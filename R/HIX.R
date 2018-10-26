#'@title Calculate HIX
#'
#'@description This function works with the output of PARAFAC.cube.design and calculate HIX.
#'@references Humification index (HIX) from Ohno et al (2002)
#'


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
      
      #Sum all data, except for emission = 346nm
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


