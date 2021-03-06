MultivariateSAR<-function(DataFrame, Neighbor, startDVcolnum, endDVcolnum, IVlist){
	#Spatial Regression Decision tree on dataframe for a dependent variable and multiple independent variables at once
		#startDVcolumn: the start column number of the Dependent Variable
	#startDVcolumn: the ending column number of Dependent Variables
	#DVname: Name of dependent variable column
	#DataFrame: R data frame/table
	#Neighbor: spatial weights matrix file from Geoda. from spdep read.gal or read.gwt2nb, left as type nb
	#IVlist should resemble "MedAge + BachelorAb + etc." Variable length.
	

	outTable <- c()
    res <- NULL
    rescolnames <- c()

	for(m in startDVcolnum:endDVcolnum)
	{
		DVname<-colnames(DataFrame)[m]
		formulaReg<-formula(paste(DVname, "~", IVlist))

		formulaReg
        rescolnames <- cbind(rescolnames, DVname)
		OutputName<-paste(DVname,"_x_","MultipleVariables",".txt", sep="")

		#Run Spatial Regression Decision Function

		cat("\nRunning Spatial Regression Decision with: ")
		print(formulaReg)
		cat("\n")

		output <- SpatialRegressionDecision_Multivariate(formulaReg, DataFrame, Neighbor, OutputName, outTable, res)
		outTable <- output$outTable
        res <- output$res
        cat("\n\n\n\n\n\n")
	}
    tablenames <-  c("call", "Model_type", "Intercept", "OLS_AdjustR^2", "SAR_PseudoR^2", "loglik/lm_loglik", "AIC/LM_AIC", "lambda_rho", "Wald_Test", "LR_Test", "BPTest", "LMError")
    for (IV in strsplit(IVlist, split="+", fixed=TRUE)) {tablenames <- c(tablenames, IV)}
    colnames(outTable) <- tablenames
    write.csv(file=paste("MultivariateSAR.csv"), outTable)
    colnames(res) <- rescolnames
    multivariate_residuals <<- res
}
