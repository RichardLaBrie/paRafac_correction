files=data.frame(paths=paste0("data/",list.files("data",recursive=T)))
write.csv(files,"exemple_paths.csv")
