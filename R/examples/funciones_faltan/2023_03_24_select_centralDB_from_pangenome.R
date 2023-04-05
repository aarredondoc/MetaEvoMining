library(dplyr)
library(readr)

#search_shell_enzymes_DB<-function(Pangenomematrix_csv){

# Pangenome_matrix <- read_csv(Pangenomematrix_csv)
  Pangenome_matrix <- read_csv("pangenome_matrix_t0.csv")
  cols_to_keep<-grep("*.faa", names(Pangenome_matrix), value= TRUE)
  matrix_subset<-Pangenome_matrix[cols_to_keep]
  #cut_matrix <- Pangenome_matrix[,15:26]
  row1<-matrix_subset[1,]

  makerow_ofcondition <- function (row) {
  V <- as.logical(row)
  keepvalue<-sum(V)>length(V)/2
#  make a function applied to all rows in a dataframe
# Setting row names from a column
  return(keepvalue)
  }


#  make a function applied to all rows in a dataframe

allrow_values<-apply(matrix_subset,MARGIN = 1, makerow_ofcondition)
#allrow_values
matrix_subset$gene<-Pangenome_matrix$Gene
matrix_subset$more_than_half<-allrow_values
matrix_subset<-filter(matrix_subset,more_than_half == TRUE)

#write.table(matrix_subset , file =  "data/shell_enzymes_table.csv", sep = "\t", dec = ".",
#            row.names = FALSE, col.names = FALSE, quote=FALSE)
#write.csv(matrix_subset,file = "data/shell_enzymes_table.csv", row.names = FALSE)
#read_csv("data/shell_enzymes_table.csv")
#return(shellenzymes)
#}
#search_shell_enzymes_DB("pangenome_matrix_t0.csv")
#shellenzymes<-read_tsv("data/shell_enzymes_table.csv")


libreria #stringi
