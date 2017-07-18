#'@title Superimpose all absorbance scan
#'
#'@description This function reads all files in /CDOM and plot them.
#'@param pdf is a logical paramter to export curves in a pdf file. Default is F

#'@export

CDOMOverlay <- function(pdf = F)
{
	#file.data = choose.files(caption="Select CDOM file(s)")
	
  file.data = list.files(path = "./data/CDOM/")
	CDOM = list()
	for(i in 1:length(file.data))
	{
		data = read.table(paste("./data/CDOM/", file.data[i], sep=""), skip = 1, header = 1, sep=",")
		WV = data[,1]
		abs = data[,2]
		CDOM[[i]] = cbind(WV, abs)
	}
	if(pdf) pdf("CDOM_courbes.pdf")
	for(i in 1:length(CDOM))
	{
    CDOM.temp = CDOM[[i]][,2]
		if(i == 1)
		{
			plot(CDOM.temp ~ CDOM[[i]][,1], xlim = c(190, 900), ylim = c(0, 3), type = "l", main = "")
		}
		else
		{
			lines(CDOM.temp ~ CDOM[[i]][,1])
		}
	}
  if(pdf) dev.off()
}

