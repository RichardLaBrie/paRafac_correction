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
