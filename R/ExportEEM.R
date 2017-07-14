#'@title This function export your data into 1 single txt files that will be incorporated in Matlab for the PARAFAC
#'
#'@description This function export in text file the output of PARAFAC.cube.design()
#'It is possible to cut the cube if all excitation, emission or samples are not required

#'@param cube is the output of PARAFAC.cube.design()
#'@param cut is the number of excitation, emission and sample to remove from cube, respectively. Default is c(0,0,0)
#'@export

#

ExportEEM <- function(cube, cut = c(0,0,0))
{
	ExCut = 0
	EmCut = 0
	EEMCut = 0
	if(cut[1] > 0)
	{
	  ExCut = c(1,cut[1])
		cube[[1]] = cube[[1]][-ExCut,,]
		cube[[3]] = cube[[3]][-ExCut]
	}
	if(cut[2] > 0)
	{
		EmCut = c(1:cut[2])
		cube[[1]] = cube[[1]][,-EmCut,]
		cube[[4]] = cube[[4]][-EmCut]
	}
	if(cut[3] > 0)
	{
		EEMCut = c(1:cut[3])
		cube[[1]] = cube[[1]][,,-EEMCut]
	}
	setwd(getwd())
	write.table(cube[[1]], "DataCube.txt")
	write.table(cube[[2]], "DataName.txt")
	write.table(cube[[3]], "ExcitationSequence.txt")
	write.table(cube[[4]], "EmissionSequence.txt")
}
