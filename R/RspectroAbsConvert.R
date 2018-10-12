#'@title RspectroAbs converter to csv files
#'
#'@description Transforms Rdata files created by RspectroAbs package developed 
#'by Simon Bélanger into csv files usable by this package
#'@param Rdatapath is the path to you Rdata folder created by run.process.ag.batch(). Default is "./data/Rdata/"
#'@param CDOMpath is the path to your CDOM folder used for this package. Default is "./data/CDOM/"

#'@export

RspectroAbsConvert = function(Rdatapath = "./data/Rdata/", CDOMpath = "./data/CDOM/")
{
  path = Rdatapath
  allRdata = list.files(path)

  for(i in 1:length(allRdata))
  {
    load(paste0(path,allRdata[i]))
    output = as.matrix(data.frame(nm = Ag$Lambda, Abs = Ag$Ag.offset))
    write.csv(x = output, file = paste0(CDOMpath,Ag$ID,".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

}