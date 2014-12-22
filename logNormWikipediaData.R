logNormWikipediaData <- function(dataframe, startcolnum, endcolnum, popcolnum) {
#loops through data, log transforms per capita, sets -inf values to min

	for(i in startcolnum:endcolnum){
		dataframe[i] <- log10(dataframe[i]/dataframe[popcolnum])
		realmin <- min(dataframe[dataframe[i] != -Inf, i])
		dataframe[dataframe[i] == -Inf, i] <- realmin
		dataframe[i] <- scale(dataframe[i])
	}
	
	return(dataframe)
}