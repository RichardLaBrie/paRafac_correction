#'@title This function fills gap in missing nanometers between CDOM and FDOM matching file
#'
#'@description If CDOM files are short in absorbance at low or high wavelength compared to EEMs
#'this function will create dummy values to fill the gap and let the function apply the correction
#'These wavelength must be removed from the EEMs
#'
#'@param CDOM is the CDOM file to correct
#'@param low is a logical parameter to let the function add short wavelengths.
#'@param high is a logical parameter to let the function add long wavelengths.
#'@param lowfrom is the start of the short wavelength
#'@param lowto is the end of the short wavelength
#'@param highfrom is the start of the long wavelength
#'@param highto is the end of the long wavelength


CDOMFileCorrection <- function(CDOM, low, high, lowfrom = 0, lowto = 0, highfrom = 0, highto = 0)
{
	if(low) low.seq = seq(lowfrom, lowto, 1)
	if(high) high.seq = seq(highfrom, highto, 1)	
	if(low) output = matrix(low.seq, byrow = F, ncol = 2, nrow = length(low.seq))
	if(high) output2 = matrix(high.seq, byrow = F, ncol = 2, nrow = length(high.seq))
	if(low) CDOM = rbind(output, CDOM)
	if(high) CDOM = rbind(CDOM, output2)
	return(CDOM)
}