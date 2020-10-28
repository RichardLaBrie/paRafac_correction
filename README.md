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

## Validate and export your data:
You can see all your corrected EEMs using PlotAllEEM.R on your R plots or by exporting a pdf file containing all graphs.

Make sure that all cdom and fdom files have been correctly assigned to each other (look in verifyMatches.csv).

Export your data cube, ready to be imported into Matlab with ExportEEM.R

## How to load the data in Matlab:
1. Import data (DataCube in next steps)
2. In _Range_, select B2: XYZ (don't change the end cell)
3. Just right of this, select _Numeric Matrix_
4. Import Selection
5. Repeat steps for emission (Em in next steps) and excitation (Ex in next steps) sequences, but choose _Column vectors_

```Matlab
nEx = size(Ex,1); %Gives the number of excitation you use
nEm = size(Em,1); %Gives the number of emission you use
nSample = size(DataCube,2)/nEm; %Gives the number of sample you have
Data = DataCube;
X = reshape(DataCube, nEx, nEm, nSample); %Reshape the matrix to create a cube
X = permute(X, [3 2 1]); %Rotate the cube in the correct order for EEMs
```
From here, you can follow commands from DrEEM toolbox (Murphy K.R., Stedmon C.A., Graeber D. and R. Bro, Fluorescence spectroscopy and multi-way techniques. PARAFAC, Anal. Methods, 2013, DOI:10.1039/c3ay41160e.)
