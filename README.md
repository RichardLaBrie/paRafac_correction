[![DOI](https://zenodo.org/badge/89527851.svg)](https://zenodo.org/badge/latestdoi/89527851)  
# paRafac_correction
The 'data' folder contains examples of how files and folders should be organized, with matching data between CDOM and FDOM files.
The 'MissnamedData' folder is an example where data files are properly stuctured while having a missmatch between a CDOM and a FDOM file. To identify which file is wrongly associated, look at 'verifyMatches.csv'.

## How to install the package
if (!require("devtools")) install.packages("devtools")

library("devtools")  
devtools::install_github("RichardLaBrie/paRafac_correction",ref = "Master")  
library("paRafac.correction")  

## Download and try the functions  
set your working directory  
Download and save in working directory (https://github.com/RichardLaBrie/paRafac_correction/blob/Development/data.zip)  
unzip("./data.zip")  

This will produce the output with the 25 samples given as example  
cube=PARAFAC.cube.design(dot.number = 2)

## Structure of the folders:
The 'excitation and emission correction' .csv files (Excorr and Emcorr, respectively) have to be in the same folder as the .Rproj file.

FDOM, CDOM and nano files must be in their respective repositories and must respect the lower/upper case format (uppercase for CDOM and FDOM; lowercase for nano). All FDOM and CDOM files must have the same name.

->data

-> ->CDOM  
-> ->FDOM  
-> ->nano
