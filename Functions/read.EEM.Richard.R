#'@title Reads CSV file of EEMs produced by the Cary Eclipe fluorometer
#'
#'@description This function reads a CSV file produced by the Cary Eclipe fluorometer
#'
#'@param excitation is a vector of three variables of the scanning setup (min, max, interval).
#'Default is c(220, 450, 5)
#'@param emission is a vector of three variables of the scanning setup (min, max, interval).
#'Default is c(230, 600, 2)
#'@param EMCOL is a logical parameter indicating whether or not the emission are
#'stored as column in the csv file. Default is FALSE.
#'@param samplepercsv is a parameter which indicates the number of sample
#'@param split is a parameter to define the symbol between your samples name if samplepercsv > 1.
#'Default is "_"
#'
#'@seealso \code{\link{plot.EEM.go}}
#'
#'@export
#'
#'
#

read.EEM.Richard <- function(filename, excitation = c(220, 450, 5), emission = c(230, 600, 2), EMCOL = FALSE, samplepercsv = 1, split = "_")
{

 # define wavelenght vectors and matrix
 wlex = seq(excitation[1], excitation[2], excitation[3])
 wlem = seq(emission[1], emission[2], emission[3])

 nex = length(wlex)
 nem = length(wlem)

 data = read.csv(filename, skip = 1)
 eem = matrix(nrow = nex, ncol = nem)
 eem2 = matrix(nrow = nex, ncol = nem)
 eem3 = matrix(nrow = nex, ncol = nem)
 eem4 = matrix(nrow = nex, ncol = nem)
 data = as.matrix(data)

 # lopp on either nem or nex
	if (EMCOL)
	{
 	x = 1
	if (samplepercsv == 1)
		{
		for (i in seq(2, (2 * nem), 2))
			{
			eem[, x] = data[, i]
			x = x + 1
			}
		}
	else
		{
		if(samplepercsv == 2)
			{
			x = 1
			for (i in seq(2, (2 * samplepercsv * nem), 4))
				{
				eem[, x] = data[, i]
				x = x + 1
				}
			x = 1
			for (j in seq(4, (2 * samplepercsv * nem), 4))
				{
				eem2[, x] = data[, j]
				x = x + 1
				}
			}
		else
			{
			if(samplepercsv == 3)
				{
				x = 1
				for (i in seq(2, (2 * samplepercsv * nem), 6))
					{
					eem[, x] = data[, i]
					x = x + 1
					}
				x = 1
				for (j in seq(4, (2 * samplepercsv * nem), 6))
					{
					eem2[, x] = data[, j]
					x = x + 1
					}
				x = 1
				for (k in seq(6, (2 * samplepercsv * nem), 6))
					{
					eem3[, x] = data[, k]
					x = x + 1
					}	
				}
			else
				{
				x = 1
				for (i in seq(2, (2 * samplepercsv * nem), 8))
					{
					eem[, x] = data[, i]
					x = x + 1
					}
				x = 1
				for (j in seq(4, (2 * samplepercsv * nem), 8))
					{
					eem2[, x] = data[, j]
					x = x + 1
					}
				x = 1
				for (k in seq(6, (2 * samplepercsv * nem), 8))
					{
					eem3[, x] = data[, k]
					x = x + 1
					}
				x = 1
				for (l in seq(8, (2 * samplepercsv * nem), 8))
					{
					eem4[, x] = data[, l]
					x = x + 1
					}
				}
			}
		}
	}
	else 
	{
		x = 1
		if (samplepercsv == 1)
		{
		for (i in seq(2, (2 * nex), 2))
			{
			eem[x, ] = data[, i]
			x = x + 1
			}
		}
	else
		{
		x = 1
		if(samplepercsv == 2)
			{
			for (i in seq(2, (2 * samplepercsv * nex), 4))
				{
				eem[x, ] = data[, i]
				x = x + 1
				}
			x = 1
			for (j in seq(4, (2 * samplepercsv * nex), 4))
				{
				eem2[x, ] = data[, j]
				x = x + 1
				}
			}
		else
			{
			if(samplepercsv == 3)
				{
				x = 1
				for (i in seq(2, (2 * samplepercsv * nex), 6))
					{
					eem[x, ] = data[, i]
					x = x + 1
					}
				x = 1
				for (j in seq(4, (2 * samplepercsv * nex), 6))
					{
					eem2[x, ] = data[, j]
					x = x + 1
					}
				x = 1
				for (k in seq(6, (2 * samplepercsv * nex), 6))
					{
					eem3[x, ] = data[, k]
					x = x + 1
					}	
				}
			else
				{
				for (i in seq(2, (2 * samplepercsv * nex), 8))
					{
					eem[x, ] = data[, i]
					x = x + 1
					}
				x = 1
				for (j in seq(4, (2 * samplepercsv * nex), 8))
					{
					eem2[x, ] = data[, j]
					x = x + 1
					}
				x = 1
				for (k in seq(6, (2 * samplepercsv * nex), 8))
					{
					eem3[x, ] = data[, k]
					x = x + 1
					}
				x = 1
				for (l in seq(8, (2 * samplepercsv * nex), 8))
					{
					eem4[x, ] = data[, l]
					x = x + 1
					}
				}
			}
		}
	}
	
	
	 # extract file name
 if (.Platform$OS.type == "unix")
 {
  if(samplepercsv == 1)
  {
  x = unlist(strsplit(filename, '/'))
  ix = length(x)
  }
  else
  {
  x = unlist(strsplit(filename, "/"))
  ix = length(x)
  y = unlist(strsplit(x[ix], ".", fixed = T))[2] #était à [1]
  z = strplit(y, "split")
  }
 }
 else
 {
  if(samplepercsv == 1)
  {
  x = unlist(strsplit(filename, "\\", fixed = T))
  ix = length(x)
  z = unlist(strsplit(x[ix], ".", fixed = T))[2] #était à [1]
  }
  else
  {
  x = unlist(strsplit(filename, "\\", fixed = T))
  ix = length(x)
  y = unlist(strsplit(x[ix], ".", fixed = T))[2] #était à [1]
  z = unlist(strsplit(y, split))
  }
 }
	
	 if(samplepercsv == 1)
		{
		 data = list(EEM.name = list(name1 = z), EEM.list = list(eem = eem), wlex = wlex, wlem = wlem)
		}
	 else
		{
		 if(samplepercsv == 2)
			{
			 data = list(EEM.name = list(name1 = z[1], name2 = z[2]), EEM.list = list(eem = eem, eem2 = eem2), wlex = wlex, wlem = wlem)
			}
		 else
			{
			 if(samplepercsv == 3)
			 {
			 data = list(EEM.name = list(name1 = z[1], name2 = z[2], name3 = z[3]), EEM.list = list(eem = eem, eem2 = eem2, eem3 = eem3), wlex = wlex, wlem = wlem)
			 }
			 else
		 {
				 data = list(EEM.name = list(name1 = z[1], name2 = z[2], name3 = z[3], name4 = z[4]), EEM.list = list(eem = eem, eem2 = eem2, eem3 = eem3, eem4 = eem4), wlex = wlex, wlem = wlem)
			 }
			}
		}
	 return(data)

}
