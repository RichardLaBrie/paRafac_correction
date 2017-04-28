CDOMOverlay <- function()
{
	#file.data = choose.files(caption="Select CDOM file(s)")
  
	file.data = list.files(path = ".//CDOM")
	CDOM = list()
	for(i in 1:length(file.data))
	{
		data = read.table(paste(".//CDOM//", file.data[i], sep=""), skip = 1, header = 1, sep=",")
		WV = data[,1]
		abs = data[,2]
		CDOM[[i]] = cbind(WV, abs)
	}
	pdf("CDOM_courbes.pdf")
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
  dev.off()
}

