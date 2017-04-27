
plot.integrate.RAMAN <- function(EEM, maxf, graph=T){

	ex350  <- which(EEM$wlex == 350)
	em370  <- which(EEM$wlem == 370)
	em380  <- which(EEM$wlem == 380)
	em414  <- which(EEM$wlem == 414)
	em428  <- which(EEM$wlem == 428)

	# Substract baseline
	Df = data.frame(x = c(EEM$wlem[em370:em380], EEM$wlem[em414:em428]), y = c(EEM$EEM.list$eem[ex350,em370:em380,1], EEM$EEM.list$eem[ex350,em414:em428,1]))
	mod = lm(y~x, data=Df)
  offset = predict(mod, newdata = data.frame(x = EEM$wlem[em370:em428]))
	raman = EEM$EEM.list$eem[ex350,em370:em428,1] - offset
	
	# integration of the Raman peak folowwing LAWAETZ and STEDMON (2009)
	fx.linear <- approxfun(EEM$wlem[em370:em428], raman)
	res <- integrate(fx.linear, 371, 428)[1]
	RamanInt <- as.numeric(res)
	
  # Graph production
  
	if(graph)
	{
		plot(EEM$wlem[em370:em428], EEM$EEM.list$eem[ex350,em370:em428,1],
			type="l", lwd=3,
			xlab="Emission wavelenght", ylab="Fluorescence")
		abline(mod, lwd = 3, col = "blue")
		text(380,20, paste("Raman integration:", as.character(signif(RamanInt,5))))
		# dev.off()
	}
	return(RamanInt)
}
