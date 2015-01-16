if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep")}
library(spdep)

source("SpatialRegressionDecision/AssignSignificantCode.R")
source("SpatialRegressionDecision/DrawDistribution.R")
source("SpatialRegressionDecision/logNormWikipediaData.R")
source("SpatialRegressionDecision/MultivariateSAR.R")
source("SpatialRegressionDecision/Prepare_MergeUGCdata.R")
source("SpatialRegressionDecision/SpatRegrDecision_csvoutput.R")
source("SpatialRegressionDecision/UnivariateSAR_csvoutput.R")

source("AssignSignificantCode.R")
source("DrawDistribution.R")
source("logNormWikipediaData.R")
source("MultivariateSAR.R")
source("Prepare_MergeUGCdata.R")
source("SpatRegrDecision_csvoutput.R")
source("UnivariateSAR_csvoutput.R")

usc <- read.csv("inputdata/usc_basedata.csv", header=TRUE)
usc7nn <- read.gwt2nb("inputdata/USCounties_7nn.gwt", region.id=usc$FIPS)

UnivariateSAR(DataFrame = usc, Neighbor = usc7nn, startDVcolumn = 23, endDVcolumn = 24, startIVcolnum = 7, endIVcolnum - 13)

logScaleZeroesToMinNegSkew <- function(dataframe, startcolnum, endcolnum) {
  #@params
  #dataframe: R data frame
  #startcolnum: column index number of first column in dataframe to be transformed
  #endcolnum: column index number of last column in dataframe to be transformed
  
  for(i in startcolnum:endcolnum) {
    dataframe[i] <- log10(dataframe[i])
    if (identical(min(dataframe[i]),-Inf)) {
      realmin <- min(dataframe[dataframe[i] != -Inf, i])
      dataframe[dataframe[i] == -Inf, i] <- realmin
    }
    dataframe[i] <- scale(dataframe[i])
  }
  return(dataframe)
}


logScaleZeroesToMinNegSkewSqrt <- function(dataframe, startcolnum, endcolnum) {
  #@params
  #dataframe: R data frame
  #startcolnum: column index number of first column in dataframe to be transformed
  #endcolnum: column index number of last column in dataframe to be transformed
  
  for(i in startcolnum:endcolnum) {
    dataframe[i] <- sqrt(dataframe[i])
    dataframe[i] <- scale(dataframe[i])
  }
  return(dataframe)
}


logScaleZeroesToMinPosSkew <- function(dataframe, startcolnum, endcolnum) {
  #@params
  #dataframe: R data frame
  #startcolnum: column index number of first column in dataframe to be transformed
  #endcolnum: column index number of last column in dataframe to be transformed
  
  for(i in startcolnum:endcolnum) {
    k = max(dataframe[i]) + 1
    dataframe[i] <- log10(k - dataframe[i])
    if (identical(min(dataframe[i]),-Inf)) {
      realmin <- min(dataframe[dataframe[i] != -Inf, i])
      dataframe[dataframe[i] == -Inf, i] <- realmin
    }
    dataframe[i] <- scale(dataframe[i])
  }
  return(dataframe)
}

logScaleZeroesToMinPosSkewSqrt <- function(dataframe, startcolnum, endcolnum) {
  #@params
  #dataframe: R data frame
  #startcolnum: column index number of first column in dataframe to be transformed
  #endcolnum: column index number of last column in dataframe to be transformed
  
  for(i in startcolnum:endcolnum) {
    max = max(dataframe[i]) + 1
    dataframe[i] <- sqrt(max - dataframe[i])
    dataframe[i] <- scale(dataframe[i])
  }
  return(dataframe)
}
