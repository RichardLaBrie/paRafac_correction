#'@title Extract CDOM value at chosen nm
#'
#'@description This function reads any number of CDOM files and select an absorbance value.

#'@param FileSelect is a logical argument for the number of files to load. Default is FALSE, meaning that the function
#will automatically load and calculate S for all CDOM files "./data/CDOM"
#If TRUE, you get to choose the files you want
#'@param nm1 is the desired wavelength. Default is 340
#'@param skip is the number of lines to skip before getting the header in CDOM files


#'@export

Abs.val <- function(data.file = "data", FileSelect = F, skip = 1, nm = 340)
{
if(!FileSelect) file.dir = list.files("./data/CDOM")
if(FileSelect) file.dir = choose.files("./data/CDOM")

CDOM340 = matrix(0, nrow = length(file.dir), ncol=1)
rownames(CDOM340) = file.dir

for(i in 1:length(file.dir)) 
{
  CDOM = read.table(paste0("./",data.file,"/CDOM/",file.dir[i]), skip = skip, header = skip + 1, sep=",")
  WL = CDOM[,1]
  
  CDOM340[i] = subset(CDOM[,2], WL == nm)
}

#Return result
return(CDOM340)

}