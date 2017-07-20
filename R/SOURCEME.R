if(!require(stringr)) install.packages("stringr")
library("stringr")
if(!require(stringdist)) install.packages("stringdist")
library("stringdist")
if(!require(devtools)) install.packages("devtools")
library("devtools")
temp <- list.files(path = "./R")
temp = temp[-grep("SOURCEME",temp)]
for(i in 1:length(temp))
{
  source(paste0("./R/",temp[i]))
}
