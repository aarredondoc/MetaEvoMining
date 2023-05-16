#' @title filter for expanded families in the input files
#' @description This function searches the EvoMining table and looks for columns
#' (enzymes) where the counts in the input genomes are above the mode. It
#' reports those columns in a list to run the trees.
#' @usage filter_interest_families(EvoMinining_heatplot)
#' @param EvoMinining_heatplot is a copy count table where the columns are
#' enzymes and the rows are the input genomes
#' @details This function is part of a package used for the analysis of
#' bins metabolism.
#' @import dplyr rlang readr plyr
#' @examples
#' filter_interest_families("results/Oleiphillaceae_evomining_table.tsv")
#' @export


filter_interest_families<-function(EvoMinining_heatplot){
#load variables------------------------------------------------------------------####
evominining_table <- read_tsv(EvoMinining_heatplot)#dataframe
Totalgenomes<- evominining_table$Genomes #all genome names list
Mydata_bins <- grep("*mSIPHEX*",Totalgenomes) # My genomes of interest
#encontrar la moda de una columna-------------------------------------------####
getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#select all numeric columns-------------------------------------------------####
numerical_columns <- sapply(evominining_table, is.numeric)
#class(numerical_columns)
numco<-evominining_table[numerical_columns]#selecciona todas las columnas numericas
#function getmoda applied to all columns of a dataframe---------------------####
allmodes<-sapply(numco,getmoda)#lista de nombres de columnas y las modas de todas las columnas

#funtion filter_EF_above_mode applied to all columns of a dataframe----------------------------####

keepindex<-sapply(numco,filter_EF_above_mode,Mydata_bins)#arroja una lista de nombres con TRUE si esta en los genomas de interes y FALSE si no está
keepcolumns<-numco[keepindex] #familias a las que si hacer el arbol de evomining
nameskeepcol<-colnames(keepcolumns)

#output name

file_name_var <- EvoMinining_heatplot
fam_nm<-strsplit(file_name_var,"_")
fam_name <-fam_nm[[1]][1]
#create a file with the EF that we want to keep-----------------------------####
write.table(nameskeepcol, file= paste(fam_name,"_runtreeevo_AAD_DB.txt", sep = ""), sep = "\t", row.names = FALSE, col.names = FALSE,quote = FALSE)


}

filter_interest_families("results/Oleiphillaceae_evomining_table.tsv")
