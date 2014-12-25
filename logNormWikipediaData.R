logNormWikipediaData <- function(dfMaster, dfNewVgi, MasterFK, VgiFK, newVgiStartColNum, newVgiEndColNum, TotalPopColName="TotalPop", suffix="_new", onlyMergeVgiStartCol=NULL, onlyMergeVgiEndCol=NULL) {
  #loops through data, log transforms per capita, sets -inf values to min

	#@params
	#dfMaster: the scaled sociodemographic data frame
	#dfNewVgi: data frame with new aggregated VGI data to be merged in and transformed
	#MasterFK: column name within dfMaster that will be used to match during the merge (e.g. "FIPS" or "OBJECTID")
  #VgiFK: column name within dfNewVgi that will be used to match during the merge (e.g. "FIPS" or "Universal.ID")
	#newVgiStartColNum: starting column number for new VGI data to be added (e.g. first column of data after FIPS)
	#newVgiEndColNum: ending column number for new VGI data to be added
	#TotalPopColName: dataframe column name with the population data from dfMaster
	#suffix: suffix to append to for attributes in dfNewVgi, should be the {Language} e.g. "ZH"
  #onlyMergeVgiStartCol: first column number for attributes like percentages that should not be transformed or normalized
  #onlyMergeVgiEndCol: last column number for attributes like percentages that should not be transformed or normalized

  nrowMaster = nrow(dfMaster)

  #Append total population column of each administrative area
  dfNewVgi <- merge(dfNewVgi, dfMaster[c(MasterFK,TotalPopColName)], by.x=VgiFK, by.y=MasterFK, all.y=TRUE)
  dfNewVgi[is.na(dfNewVgi)] <- 0
  if (nrowMaster != nrow(dfMerged)) {
      stop(paste("output missing", nrowMaster - nrow(dfMerged),"rows"))
  }

  #Takes log10 transformation, sets zero values to the minimum, and scales the resulting so the mean is set to zero and values correspond to standard deviations
	for(i in newVgiStartColNum:newVgiEndColNum) {
		dfNewVgi[i] <- log10(dfNewVgi[i]/dfNewVgi[,TotalPopColName])
    if (identical(min(dfNewVgi[i]),-Inf)) {
      realmin <- min(dfNewVgi[dfNewVgi[i] != -Inf, i])
      dfNewVgi[dfNewVgi[i] == -Inf, i] <- realmin
    }
    dfNewVgi[i] <- scale(dfNewVgi[i])
	}

  #Also merges additional data columns like percentages that should not be normalized or scaled
  if (!is.null(onlyMergeVgiStartCol)) {
    dfMerged <- merge(dfMaster, dfNewVgi[c(newVgiStartColNum:newVgiEndColNum, onlyMergeVgiStartCol:onlyMergeVgiEndCol, match(VgiFK, names(dfNewVgi)))], by.x=MasterFK, by.y=VgiFK, suffixes= c("",suffix))

    return(dfMerged)
  }

  #Merges newly transformed data into original dataset
	dfMerged <- merge(dfMaster, dfNewVgi[c(newVgiStartColNum:newVgiEndColNum, match(VgiFK, names(dfNewVgi)))], by.x=MasterFK, by.y=VgiFK, suffixes= c("",suffix))

	return(dfMerged)
}
