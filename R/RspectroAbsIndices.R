#'@title Summarize RspectroAbs CDOM parameters
#'
#'@description Summarizes all CDOM indices calculated with RspectroAbs package developed 
#'by Simon Bélanger and export them in csv
#'@param Rdatapath is the path to you Rdata folder created by run.process.ag.batch(). Default is "./data/Rdata/"
#'@param export is a logical paramater to export a matrix with all CDOM indices. Default is FALSE

#'@export

RspectroAbsIndices <- function(RDatapath = "./data/Rdata/", export = F)
{
  allRdata = list.files(RDatapath) #Creates a vector with all files name
  
  Output = matrix(nrow = length(allRdata), ncol = 6)
  colnames(Output) = c("ID", "a440", "S275_295", "S350_400", "S350_500", "Sr")
  
  for(i in 1:length(allRdata))
  {
    load(paste0(RDatapath,allRdata[i])) #Load a RData file in R
    Output[i,1] = as.numeric(Ag$ID)
    Output[i,2] = as.numeric(Ag$a440)
    Output[i,3] = as.numeric(Ag$S275_295)
    Output[i,4] = as.numeric(Ag$S350_400)
    Output[i,5] = as.numeric(Ag$S350_500)
    Output[i,6] = as.numeric(Ag$Sr)
  }
  if(export) write.csv(x = Output, file = "CDOMIndices.csv", row.names = FALSE, fileEncoding = "UTF-8")
  return(Output)
}
