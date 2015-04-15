SpatialRegressionDecision_Multivariate<-function(Formula, DataFrame, Neighbor, OutputName, outTable){
	#Function to make decision and run the proper spatial regression model
	
	#Argument Specification
	#Runs through logical decision tree for choosing appropriate univariate (spatial) regressions with Wikipedia data
	#Formula: e.g. CRIME ~ INC + HOVAL
	#DataFrame: R data frame/table
	#Neighbor: spatial weights matrix file from Geoda. from spdep read.gal or read.gwt2nb, left as type nb
	#OutputName: in quotations, will be created in wd or will overwrite existing file

	#Reload the spdep in case it is not loaded
	library(spdep)
	
	#Running the OLS
	cat("Running Classic OLS Regression...\n")
	OLS<-lm(Formula, data=DataFrame)
	
	#Write the formula
	capture.output(print(Formula),file=OutputName, append=TRUE)
	cat("\n\n\n", file=OutputName, append=TRUE)
	
	#Write OLS Regression Result to the file
	cat("==============================\nOLS Regression Result:\n",file=OutputName, append=TRUE)
	capture.output(summary(OLS),file=OutputName, append=TRUE)
	cat("==============================\n\n\n", file=OutputName, append=TRUE)
	
	#Build the spatial weight list
	ListW<-nb2listw(Neighbor)
	
	#Moran's I test for spatial autocorrelation in OLS residual
	cat("Running Moran's I test on Classic OLS Regression's residual ...\n")
	ResidualMoranTest<-lm.morantest(OLS, ListW)
	
	#print(ResidualMoranTest)
	
	ResidualMoranP<-ResidualMoranTest$p.value
	
	cat("==============================\nMoran's I test against the OLS regression residual:\n",file=OutputName, append=TRUE)
	
	#Checks for significance in spatial autocorrelation
	if(ResidualMoranP<0.05)
	{
		cat("P-value for OLS Regression Residual Moran's I test is: ", ResidualMoranP, ", and it's significant. Run Lagrangian Multipler Test to decide which SAR model to use\n")
		capture.output(print(ResidualMoranTest),file=OutputName, append=TRUE)
		cat("==============================\n\n\n", file=OutputName, append=TRUE)
		
		#Run Lagrangian Multipler Test
		cat("Running Lagrangian Multiplier Test ...\n")
		LagrangianTest <- lm.LMtests(OLS,ListW, test="all")
		
		#Write Lagrangian Test Result to the file
		cat("==============================\nLagrangian Multiplier Test on the OLS Regression Result:\n",file=OutputName, append=TRUE)
		capture.output(print(LagrangianTest),file=OutputName, append=TRUE)
		cat("==============================\n\n\n", file=OutputName, append=TRUE)
		
		#Output the p-value of Lagrangian Multiplier Test
		cat("p-value of LMerr is: ", LagrangianTest$LMerr$p.value, "p-value of LMlag is: ", LagrangianTest$LMlag$p.value,"\n")
		
		#LM values for both spatial lag and error models are significant	
		if(LagrangianTest$LMerr$p.value<0.05 & LagrangianTest$LMlag$p.value<0.05)
		{
			cat("Both LMerr and LMlag are significant. Compare Robust LMerr and Robust LMlag\n")
			cat("p-value of Robust LMerr is: ", LagrangianTest$LMerr$p.value, "... p-value of Robust LMlag is: ", LagrangianTest$LMlag$p.value, "\n")
			
			#If both LMerr and LMlag are significant, check if RLMerr or RLMlag are significant
			if(LagrangianTest$RLMerr$p.value<0.05 | LagrangianTest$RLMlag$p.value<0.05)
			{
				if(LagrangianTest$RLMlag$p.value < LagrangianTest$RLMerr$p.value)
				{
					cat("Robust LMlag is more significant! Use SAR Lag Model\n")
					SARResult<-lagsarlm(Formula, data=DataFrame, ListW)
				}
				else
				{
					cat("Robust LMerror is more significant! Use SAR Error Model\n")
					SARResult<-errorsarlm(Formula, data=DataFrame, ListW)
				}
			}
			#Neither RLMerror nor RLMlag are significant. The more significant model is still run for data.
			else
			{
				cat("Neither Robust are significant. Model misspecification. Smaller p-value will be run but results should be evaluated.")
				cat("==============================\nNeither Robust are significant. Model misspecification. Smaller p-value will be run but results should be evaluated:\n",file=OutputName, append=TRUE)
				cat("==============================\n\n\n", file=OutputName, append=TRUE)
				
				if(LagrangianTest$RLMerr$p.value < LagrangianTest$RLMlag$p.value)
				{
					SARResult<-errorsarlm(Formula, data=DataFrame, ListW)
				}
				else
				{
					SARResult<-lagsarlm(Formula, data=DataFrame, ListW)
				}
			}
		}
		
		#LM values for only one of the lag or error models is significant
		else if(LagrangianTest$LMerr$p.value<0.05 | LagrangianTest$LMlag$p.value<0.05)
		{
			#Choose the SAR model with smaller LM p-value
			cat("Only one of LMerr and LMlag is significant. The SAR model with smaller p-value will be used.\n")
			if(LagrangianTest$LMlag$p.value<0.05)
			{
				cat("LMlag is more significant! Use SAR Lag Model\n")
				SARResult<-lagsarlm(Formula, data=DataFrame, ListW)
			}
			else if(LagrangianTest$LMerr$p.value<0.05)
			{
				cat("LMerror is more significant! Use SAR Error Model\n")
				SARResult<-errorsarlm(Formula, data=DataFrame, ListW)
			}
		}
		
		#Neither LM values for lag nor error were significant. OLS result retained.
		else
		{
			cat("Even though Residual Moran's I was significant (", ResidualMoranP, "), neither LMerr (", LagrangianTest$LMerr$p.value, ") or LMlag (", LagrangianTest$LMlag$p.value, ") are significant. Stick with OLS.\n")
			cat("==============================\n","Even though Residual Moran's I was significant (", ResidualMoranP, "), neither LMerr (", LagrangianTest$LMerr$p.value, ") or LMlag (", LagrangianTest$LMlag$p.value, ") are significant. Stick with OLS.\n", file=OutputName, append=TRUE)
			cat("==============================\n\n\n", file=OutputName, append=TRUE)
			SARResult<-OLS
		}
	}
	
	#Moran's I does not indicate spatial autocorrelation in OLS residuals
	else
	{
		cat("P-value for OLS Residual Moran's I test is: ", ResidualMoranP, ", and it's NOT significant. Stop and use OLS\n")
		cat("==============================\n","P-value for OLS Residual Moran's I test is: ", ResidualMoranP, ", and it's NOT significant. Stop and use OLS\n", file=OutputName, append=TRUE)
		SARResult<-OLS
	}	
	
	#Write the Final Regression Result to the file
	cat("==============================\n The Final Regression Result:\n",file=OutputName, append=TRUE)
	capture.output(summary(SARResult),file=OutputName, append=TRUE)
	cat("==============================\n\n\n", file=OutputName, append=TRUE)
	
	SARResult
	cat(SARResult$type)
	#Decide if SAR or OLS is run and construct the output Table
	if(!is.null(SARResult$type))
	{
		#Parse the DV Name from the formula
		DVname<-paste0(as.list(attr(terms(Formula), "variables"))[2])
			
		#Compute the Pseudo-R^2
		SARResult.fitted<-fitted.values(SARResult)
		SARResult.lm<- lm(DataFrame[,DVname] ~ SARResult.fitted)
		PseudoRSquared<-summary(SARResult.lm)$r.squared
		
		#If SAR is used, report the result
		if(SARResult$type == "error")
		{
			SpatialAutoregressiveCoefficient<-SARResult$lambda
		}
		else if(SARResult$type == "lag")
		{
			SpatialAutoregressiveCoefficient<-SARResult$rho
		}
	
		EstimatepValue<-2 * (1 - pnorm(abs(SARResult$coefficients/SARResult$rest.se)))
                bp <- bptest.sarlm(SARResult)
                rc <- lm.LMtests(model = SARResult$residuals, listw=ListW)
	
		#Note that "-"/negative sign at the beginning of a cell will cause Excel to treat the cell as a formula. To avoid this, a space is added at the beginning of a cell
		outTable<- rbind(outTable, c(Formula, SARResult$type, paste0(round(SARResult$coefficients[1],4), Helper_AssignSignCode(EstimatepValue[1])), round(summary(OLS)$adj.r.squared,4), round(PseudoRSquared, 4), paste0(" ",round(SARResult$LL,4),"/", round(SARResult$logLik_lm.model,4)), paste0(round(AIC(SARResult),4),"/",round(SARResult$AIC_lm.model,4)), round(SpatialAutoregressiveCoefficient,4), paste0(round(Wald1.sarlm(SARResult)$statistic,4),Helper_AssignSignCode(Wald1.sarlm(SARResult)$p.value)),paste0(round(LR1.sarlm(SARResult)$statistic,4),Helper_AssignSignCode(LR1.sarlm(SARResult)$p.value)),paste0(bp$statistic, Helper_AssignSignCode(bp$p.value)), paste0(round(rc$LMErr$statistic[1],4),Helper_AssignSignCode(rc$LMErr$p.value[1])))
		for (i in 2:length(SARResult$coefficients)) {outTable <- cbind(outTable, paste0(round(SARResult$coefficients[i],4),Helper_AssignSignCode(EstimatepValue[i])))}
	}
	else
	{
		#If OLS is used, report partial results
		outTable<- rbind(outTable, c(Formula, "OLSResult", paste0(OLS$coefficients[2],Helper_EvaluatePvalue(summary(OLS)$coefficients[2,4])), summary(OLS)$adj.r.squared, "LogLik_PH", "AIC_PH", "SAC_PH", "WaldT_PH", "LRTest_PH"))
	}
	return(outTable)
}
