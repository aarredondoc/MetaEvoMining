#' @title Subsets the mapped KO table based on pathways.
#' @description reads the output of the mapping_ko function to filter out KOs
#' based on pathways.
#' @usage user_id()
#' @details This function is part of a package used for the analysis of
#' bins metabolism.
#' @import dplyr rlang readr plyr
#' @examples
#' filter_userID("data/gtdbtk.bac120.summary.tsv",data_names="700mSIPHEX2_9")
#' @export
library("readr")
library("dplyr")
filter_interest_families<-function(EvoMinining_heatplot){
  #variables------------------------------------------------------------------####
  evominining_table <- read_tsv(EvoMinining_heatplot)#dataframe
  Totalgenomes<- evominining_table$Genomes #all genome names list
  Mydata_bins <-Totalgenomes # My genomes of interest
  #encontrar la moda de una columna-------------------------------------------####
  getmoda <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  ##above_mode_fun------------------------------------------------------------####

  #input a vector that contains a column
  #input2 genomas de interes
  #output verdadero o falso si los genomas se encuentran en la columna por arriba de la moda
  above_mode_fun <- function(f,g_names){#f is a vector and df is a dataframe
    EFmode <- getmoda(f)
    abovemodeindexes <- which(f > EFmode)
    trueorfalse<-any(abovemodeindexes)
    return(trueorfalse)

  }

  #select all numeric columns-------------------------------------------------####
  numerical_columns <- sapply(evominining_table, is.numeric)
  numco<-evominining_table[numerical_columns]#selecciona todas las columnas numericas

  #function above_mode_fun applied to all columns of a dataframe----------------------------####

  keepindex<-sapply(numco,above_mode_fun,Mydata_bins)#arroja una lista de nombres con TRUE si esta en los genomas de interes y FALSE si no está
  keepcolumns<-numco[keepindex] #familias a las que si hacer el arbol de evomining
  nameskeepcol<-colnames(keepcolumns)

  #create a file with the EF that we want to keep-----------------------------####
  write.table(nameskeepcol, file= "runtreeevo_plastic_DB.txt", sep = "\t", row.names = FALSE, col.names = FALSE,quote = FALSE)


}
  EF2<-evominining_table$`LINPD_3--LINC_SPHJU`
  above_mode_fun(EF2,Mydata_bins)
#setwd()
filter_interest_families("CentralDB_pangenome - Alcanivoracaceae.tsv")
