MultivariateSAR<-function(DVname, DataFrame, Neighbor, IVlist, Outputname){
	#Spatial Regression Decision tree on dataframe for a dependent variable and multiple independent variables at once
	#DVname: Name of dependent variable column
	#DataFrame: R data frame/table
	#Neighbor: spatial weights matrix file from Geoda. from spdep read.gal or read.gwt2nb, left as type nb
	#IVlist should resemble "MedAge + BachelorAb + etc." Variable length.
	#Outputname = "file.txt"
	
	formulaReg<-formula(paste(DVname, "~", IVlist))
	
	#Run Spatial Regression Decision Function
	
	cat("\nRunning Spatial Regression Decision with: ", deparse(formulaReg), "\n")
	SpatialRegressionDecision(formulaReg, DataFrame, Neighbor, Outputname)
	
}
