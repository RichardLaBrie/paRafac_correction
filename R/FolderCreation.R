#'@title Creates data folder, subfolder and R project

#'@description This function creates folders and .Rproj file require by the various functions to work.
#'These steps can be done by hand instead.

#'@param WDpath the path to the working directory 
#'@param folder is the name of the folder containing subfiles parameter. Default is "data"
#'@param subfiles is the list of folder where to put cdom, fdom and nano water samples

#'@export

#Example: FolderCreation(path.to.folder = "D:/test", WorkingDirectory = "test")

FolderCreation <- function(WDpath=".", folder = "data", subfiles = c("CDOM","FDOM","nano"))
{
  if(dir.exists(folder)) stop("The data folder already exists")
  
  dir=basename(WDpath)
  if(dir=="."){dir=basename(getwd())}
  
  path <- file.path(WDpath, paste0(dir, ".Rproj"))
  template_path <- system.file("templates/template.Rproj", package = "devtools")
  file.copy(template_path, path)
  
  dir.create(file.path(WDpath,"data"), showWarnings = T)
    for(i in 1:3)
  {
    dir.create(file.path(path.to.folder,"/data",subfiles[i]), showWarnings = FALSE)
    }
  
  files=data.frame(paths=paste0("data/",list.files("data",recursive=T)))
  files$urls=paste0("https://github.com/RichardLaBrie/paRafac_correction/blob/Development/",files$paths)
  i=1
  download.file(as.character(files$urls[i]), as.character(files$paths[i]), method="libcurl")

}
