[![DOI](https://zenodo.org/badge/89527851.svg)](https://zenodo.org/badge/latestdoi/89527851)  
# paRafac_correction
The 'data' folder contains examples of how files and folders should be organized, with matching data between CDOM and FDOM files.
The 'MissnamedData' folder is an example where data files are properly stuctured while having a missmatch between a CDOM and a FDOM file. To identify which file is wrongly associated, look at 'verifyMatches.csv'.

## How to install the package
```R
if (!require("devtools")) install.packages("devtools")
library("devtools")  
devtools::install_github("RichardLaBrie/paRafac_correction")  
library("paRafac.correction")  
```
## Download and give it a try!  
Set a working directory and run the following function to create files and download the example data 
```R
FolderCreation(WDpath=".", folder = "data", subfiles = c("CDOM","FDOM","nano"),example=T)
```
Try the package main function 
```R
cube=PARAFAC.cube.design(dot.number = 2, zlim = c(0,20))
```
## Structure of the folders:
The 'excitation and emission correction' .csv files (Excorr and Emcorr, respectively) have to be in the same folder as the .Rproj file. The files available with the packages are given only as example. You should use your own correction files given with your fluorometer.

FDOM, CDOM and nano files must be in their respective repositories and must respect the lower/upper case format (uppercase for CDOM and FDOM; lowercase for nano). All FDOM and CDOM files must have the same name.

->data

-> ->CDOM  
-> ->FDOM  
-> ->nano
