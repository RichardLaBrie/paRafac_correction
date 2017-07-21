paths=as.character(paste0("data/",list.files("data",recursive=T)))
files=data.frame(paths=c(paths,"Emcorr.csv","Excorr.csv"))
write.csv(files,"example_paths.csv")

