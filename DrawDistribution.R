DrawVariableDistribution<-function(DataFrame, startIVcolnum, endIVcolnum){
	#Function to draw distribution histogram of all variables between @startIVcolnum and @endIVcolnum
	#Run this function to see if any data transformation need to be done prior to regression analysis
	
	#DataFrame: the dataframe which contains the variables whose distribution to be visualized.
	#startIVcolnum: the column of first variable to draw
	#endIVcolnum: the column of last variable to draw
	
	for(i in startIVcolnum:endIVcolnum){
		IV<-colnames(DataFrame)[i]
		
		h<-hist(as.numeric(DataFrame[i][,1]), breaks="FD", freq=FALSE, main=paste("Distribution of ", IV))

		curve(dnorm(x, mean=mean(DataFrame[i][,1]), sd=sd(DataFrame[i][,1])), add=TRUE, col="darkblue", lwd=2)
		
		OutputName<-paste(deparse(substitute(DataFrame)), paste(IV),".pdf", sep="")
		
		dev.copy2pdf(file=OutputName, onefile=FALSE)
		dev.off()
	}	
	
}
