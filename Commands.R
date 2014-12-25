install.packages("spdep")
library(spdep)

source("SpatialRegressionDecision/AssignSignificantCode.R")
source("SpatialRegressionDecision/DrawDistribution.R")
source("SpatialRegressionDecision/logNormWikipediaData.R")
source("SpatialRegressionDecision/MultivariateSAR.R")
source("SpatialRegressionDecision/Prepare_MergeUGCdata.R")
source("SpatialRegressionDecision/SpatRegrDecision_csvoutput.R")
source("SpatialRegressionDecision/UnivariateSAR_csvoutput.R")

usc <- read.csv("inputdata/usc_basedata.csv", header=TRUE)
usc7nn <- read.gwt2nb("inputdata/USCounties_7nn.gwt", region.id=usc$FIPS)

UnivariateSAR(DataFrame = usc, Neighbor = usc7nn, startDVcolumn = 23, endDVcolumn = 24, startIVcolnum = 7, endIVcolnum - 13)
