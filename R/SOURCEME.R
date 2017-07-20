if(!require(stringr)) install.packages("stringr")
library("stringr")
if(!require(stringdist)) install.packages("stringdist")
library("stringdist")
temp <- list.files(path = "./R")
temp = temp[-grep("SOURCEME",temp)]
for(i in 1:length(temp))
{
  source(paste0("./R/",temp[i]))
}
