Prepare_MergeUGCdata <- function(df_TobeMerged,dfUGC,df_TotalPop, startColumn, endColumn, suffix){
	#This function merges the new Wikidata into the existing data frame.
	#It first normalized the data using TotalPop data frame and then merge() the dataset using RowNum columns
	#Suppose the first column of dfUGC is the UID column
	
	#@params
	#df_TobeMerged: the scaled sociodemographic data frame
	#dfUGC: Dataframe containing new Wikidata of specific languages
	#startColumn: used to define column range of dfUGC
	#endColumn: used to define column range of dfUGC
	#suffix: suffix to append to for attributes in the dfUGC, should be the {Language} e.g. "ZH"
	
	
	
	dfUGC[,startColumn:endColumn]<-dfUGC[,startColumn:endColumn]/df_TotalPop$TotalPop
	
	dfUGC[,startColumn:endColumn]<-log10(dfUGC[,startColumn:endColumn])
	
	dfUGC[,startColumn:endColumn]<-scale(dfUGC[,startColumn:endColumn])
	
	suffix<-paste("_",suffix)
	
	df_Merged<-merge(df_TobeMerged, dfUGC[c(1,startColumn:endColumn)], by.x="RowNum", by.y="RowNum", sort =TRUE, suffixes = c(".x",suffix))
	
	df_Merged
}
