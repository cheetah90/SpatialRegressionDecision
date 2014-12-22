install.packages("spdep")
library(spdep)

source("AssignSignificantCode.R")
source("DrawDistribution.R")
source("logNormWikipediaData.R")
source("MultivariateSAR.R")
source("Prepare_MergeUGCdata.R")
source("SpatRegrDecision_csvoutput.R")
source("UnivariateSAR_csvoutput.R")

usc <- read.csv("usc_basedata.csv", header=TRUE)
usc7nn <- read.gwt2nb("USCounties_7nn.gwt", region.id=usc$FIPS)

UnivariateSAR(DataFrame = usc, Neighbor = usc7nn, startDVcolumn = 23, endDVcolumn = 24, startIVcolnum = 7, endIVcolnum - 13)
