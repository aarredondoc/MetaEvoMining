library(dplyr)
library(readr)
library(stringi)
setwd("C:/Users/betterlab/Desktop/git_andres/00.get_homologs_25subset")

#search_shell_enzymes_DB<-function(Pangenomematrix_csv){

# Pangenome_matrix <- read_csv(Pangenomematrix_csv)------------------------#####

path<-"Alcanivoracaceae/alg_intersection/"

  Pangenome_matrix <- read_csv(paste0(path,"pangenome_matrix_t0_Alcanivoracaceae.tr.csv"))
  cols_to_keep<-grep("*.gbk", names(Pangenome_matrix), value= TRUE)
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

# --------------------------------------------------------------------------####
shell_genes<- matrix_subset$gene
example_gene<- "1633_rlmF.faa"
paste0(getwd(),"/",path,example_gene)
setwd(path)

file_path <- file.path(getwd(),example_gene)
file_path
file.info(file_path)$is.file
#all_files <- list.files(path)
#file_list <-list.files(path,pattern = "*.faa")
#file_list
getwd()
#files_list <- list.files()
results_list <- lapply(shell_genes, function(file) {
  file_path <- file.path(getwd(),paste0(path,file))
  if (file %in%) {
    x <- stri_read_lines(file_path)
    list(file = file, matches = stri_detect_regex(x, paste(example_gene, collapse = "|")))
  } else {
    list(file = file, matches = NULL)
  }
})

results_list
