logNormWikipediaData <- function(dfMaster, dfNewVgi, foreignKey, newVgiStartColNum, newVgiEndColNum, TotalPopColName="TotalPop", suffix="_new") {
  #loops through data, log transforms per capita, sets -inf values to min

	#@params
	#dfMaster: the scaled sociodemographic data frame
	#dfMasterFK: column name within dfMaster with a suitable foreign key for joining the new data (e.g. FIPS)
	#foreignKey: column name within both dataframes that will be used to match during the merge
	#newVgiStartColNum: starting column number for new VGI data to be added (e.g. first column of data after FIPS)
	#newVgiEndColNum: ending column number for new VGI data to be added
	#TotalPopColName: dataframe column name with the population data from dfMaster
	#suffix: suffix to append to for attributes in dfNewVgi, should be the {Language} e.g. "ZH"

  dfNewVgi <- merge(dfNewVgi, dfMaster[,c[foreignKey,TotalPopColName]], by=foreignKey)

	for(i in newVgiStartColNum:newVgiEndColNum){
		dfNewVgi[i] <- log10(dfNewVgi[i]/dfNewVgi$TotalPopColName)
		realmin <- min(dfNewVgi[dfNewVgi[i] != -Inf, i])
		dfNewVgi[dfNewVgi[i] == -Inf, i] <- realmin
		dfNewVgi[i] <- scale(dfNewVgi[i])
	}

	dfMerged <- merge(dfMaster, dfNewVgi[c(1,newVgiStartColNum, newVgiEndColNum)], by=foreignKey, sort=TRUE, suffixes= c("",suffix))

	return(dfMerged)
}
