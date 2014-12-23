logNormWikipediaData <- function(dfMaster, dfNewVgi, foreignKey, newVgiStartColNum, newVgiEndColNum, TotalPopColName="TotalPop", suffix="_new") {
  #loops through data, log transforms per capita, sets -inf values to min

	#@params
	#dfMaster: the scaled sociodemographic data frame
	#dfNewVgi: data frame with new aggregated VGI data to be merged in and transformed
	#foreignKey: column name within both dataframes that will be used to match during the merge (e.g. "FIPS")
	#newVgiStartColNum: starting column number for new VGI data to be added (e.g. first column of data after FIPS)
	#newVgiEndColNum: ending column number for new VGI data to be added
	#TotalPopColName: dataframe column name with the population data from dfMaster
	#suffix: suffix to append to for attributes in dfNewVgi, should be the {Language} e.g. "ZH"

  #Append total population column of each administrative area
  dfNewVgi <- merge(dfNewVgi, dfMaster[c(foreignKey,TotalPopColName)], by=foreignKey)

  #Takes log10 transformation, sets zero values to the minimum, and scales the resulting so the mean is set to zero and values correspond to standard deviations
	for(i in newVgiStartColNum:newVgiEndColNum) {
		dfNewVgi[i] <- log10(dfNewVgi[i]/dfNewVgi[,TotalPopColName])
    if (identical(min(dfNewVgi[i]),-Inf)) {
      realmin <- min(dfNewVgi[dfNewVgi[i] != -Inf, i])
      dfNewVgi[dfNewVgi[i] == -Inf, i] <- realmin
    }
    dfNewVgi[i] <- scale(dfNewVgi[i])
	}

  #Merges newly transformed data into original dataset
	dfMerged <- merge(dfMaster, dfNewVgi[c(newVgiStartColNum:newVgiEndColNum,match(foreignKey, names(dfNewVgi)))], by=foreignKey, suffixes= c("",suffix))

	return(dfMerged)
}
