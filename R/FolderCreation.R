#'@title Creates data folder, subfolder and R project

#'@description This function creates folders and .Rproj file require by the various functions to work.
#'These steps can be done by hand instead.

#'@param path.to.folder is the path where the working directory is
#'@param WorkingDirectory is the name of the working directory
#'@param subfiles is the list of folder where to put cdom, fdom and nano water samples

#'@export

#Example: FolderCreation(path.to.folder = "D:/test", WorkingDirectory = "test")

FolderCreation <- function(path.to.folder,WorkingDirectory, subfiles = c("CDOM","FDOM","nano"))
{
  path <- file.path(path.to.folder, paste0(WorkingDirectory, ".Rproj"))
  template_path <- system.file("templates/template.Rproj", package = "devtools")
  file.copy(template_path, path)
  
  dir.create(file.path(path.to.folder,"data"), showWarnings = FALSE)
  for(i in 1:3)
  {
    dir.create(file.path(path.to.folder,"/data",subfiles[i]), showWarnings = FALSE)
  }
  
}
