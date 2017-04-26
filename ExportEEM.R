#This function export your data into 1 single txt files that will be incorporated in Matlab for the PARAFAC

ExportEEM <- function(cube,cut = c(0,0,0))
{
	ExCut = 0
	EmCut = 0
	EEMCut = 0
	if(cut[1]>0)
	{
		ExCut = c(1:cut[1])
	}
	if(cut[2]>0)
	{
		EmCut = c(1:cut[2])
	}
	if(cut[3]>0)
	{
		EEMCut = c(1:cut[3])
	}
	setwd(getwd())
	write.table(cube[[1]][-ExCut,-EmCut,-EEMCut],"DataCube.txt")
	write.table(unlist(cube[[2]]),"DataName.txt")
	write.table(cube[[3]][-ExCut],"ExcitationSequence.txt")
	write.table(cube[[4]][-EmCut],"EmissionSequence.txt")
}