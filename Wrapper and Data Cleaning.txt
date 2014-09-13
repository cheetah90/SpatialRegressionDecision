Wrapper <- function(){
	#run multiple regressions in sequence. hard-coded. change DVs and add/subtract runs as needed.
	
	UnivariateSAR("LOG_NOP_PC", usc, usc_10nn, 9, 16)
	UnivariateSAR("LOG_OUTLINK_PC", usc, usc_10nn, 9, 16)
	UnivariateSAR("LOG_TOTLEN_PC", usc, usc_10nn, 9, 16)
}


#Basic commands for prepping data:
library(spdep)

#read in data
usc <- read.csv("USCountiesComplete_9_12_14.csv", header=TRUE)

#scale independent and dependent variables
usc[2:9] <- scale(usc[2:9])
usc[21:22] <- scale(usc[21:22])

#bring in spatial weights matrix
usc_10nn <- read.gwt2nb("USCounties_KNN10.gwt")
