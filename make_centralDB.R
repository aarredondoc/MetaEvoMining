library(dplyr)
library(readr)
library(stringi)
setwd("C:/Users/betterlab/Desktop/git_andres/00.get_homologs_25subset")

path<-"Alcanivoracaceae/alg_intersection/"

example_gene<- "1415_accA_2.faa"
listgenes<- c("1415_accA_2.faa","52012_nudJ.faa")

#subset_shell_enzymes-------------------------------------------------------####
search_shell_enzymes_DB<-function(csv_matrix,path){


  #path<-"Alcanivoracaceae/alg_intersection/"

  Pangenome_matrix <- read_csv(paste0(path,csv_matrix))
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
  shell_genes<- matrix_subset$gene


  # --------------------------------------------------------------------------####
  ##Tengo un archivo con secuencias y una lista de nombres de archivos
  #"52003_glcB.faa"                   "52004_hypothetical_protein.faa"
  ##quiero la primera linea de cada archivo de la lista concatenado en un archivo
  #nuevo
  #>ID:LHAGBDID_03183 |[Alcanivorax c15]|strain|Alcanivorax_C15_015665355.prokka.gbk|
  #>hldD|2007|JADFAH010000003.1(107935):46959-48965:1 ^^ Alcanivorax c15 strain strain.
  #>|neighbours:ID:LHAGBDID_03182(-1),ID:LHAGBDID_03184(1)|neighbour_genes:fabR_2,betA|
  #MNYFVTGATGFIGRFLVGRLLKRDDTRVFALVRCGSEYKLDALRNRLGVDADRLVAIHGDINEKLLGVSKRDQDDLNGQ
  #IDHFFHLAAIYDLTADENTQRYTNIEGTRQTLKLAEKIQAGRFHHVSSIAAAGLYDGTFTENMFEEATGLDDAYLLTKH
  #ESEALVRQESKIPWRIYRPAMVVGHSETGAMDKVDGPYYLFKFIQKLKDVLPNWIPLIGVEGGHFNVVPVDF
  #filter_files<-function(file,lista) {
  #file_path <- file.path(getwd(),paste0(path,file))
  #  file_path <- file.path(paste0(getwd(),"/",path,example_gene,collapse = '|'))
  #  if (file %in% lista) {
  #    x <- stri_read_lines(file_path)
  #    matches = stri_detect_regex(x, ">ID:")
  #  }# else {
  #list(file = file, matches = NULL)
  #  }
  #}

  #filter_files(example_gene,shell_genes)

  #results_list


  # >ID|1|accA_2|Alcanivoraxc15LHAGBDID_02122

  filter_files<-function(file,lista,path) {
    #file_path <- file.path(getwd(),paste0(path,file))
    file_path <- file.path(paste0(getwd(),"/",path,file,collapse = '|'))
    if (file %in% lista) {
      x <- readLines(file_path)
      sequence<-x[2]
      first_seq <-c(x[1],x[2])
      header<-first_seq[[1]][1]
      fragments <- stri_split(as.character(header), fixed = "|",simplify = TRUE)
      specie<-gsub("\\[|\\]","",fragments[2])
      header<-paste(stri_split(fragments[1],fixed = ":",simplify = TRUE)[1],"1",gsub(" ","_",fragments[5]),paste0(gsub(" ","",specie),gsub(" ","",stri_split(fragments[1],fixed = ":",simplify = TRUE)[2])),sep="|")
      format<-c(header,sequence)
      return(format)


    }
  }
  new_fasta_file <- file("new2.fasta", "w")
  #enzyme<-filter_files(example_gene,shell_genes,path)
  # writeLines(enzyme,new_fasta_file, sep = "\n")
  #writeLines(sequence,new_fasta_file)
  #close(new_fasta_file)

  lapply(shell_genes, function(file) {
    enzyme<-filter_files(file,shell_genes,path)
    cat(paste(enzyme[1], "\n", gsub(" ","",gsub("(.{80})", "\\1\n", enzyme[2], perl=TRUE)), collapse = "",sep = ""),"\n", file = new_fasta_file, append = TRUE,sep = "")
  })

  close(new_fasta_file)
}

search_shell_enzymes_DB("pangenome_matrix_t0_Alcanivoracaceae.tr.csv",path)

# >ID|1|accA_2|Alcanivoraxc15LHAGBDID_02122------------------------------#####

#file_path <- file.path(paste0(getwd(),"/",path,example_gene,collapse = '|'))
#if (example_gene %in% shell_genes) {
#  x <- readLines(file_path)
#  sequence<-x[2]
#  first_seq <-c(x[1],x[2])
#  header<-first_seq[[1]][1]
#  fragments <- stri_split(as.character(header), fixed = "|",simplify = TRUE)
#  specie<-gsub("\\[|\\]","",fragments[2])
#  header<-paste(stri_split(fragments[1],fixed = ":",simplify = TRUE)[1],"1",fragments[5],paste0(gsub(" ","",specie),stri_split(fragments[1],fixed = ":",simplify = TRUE)[2]),sep="|")
#  new_fasta_file <- file("new1.fasta", "w")
#  writeLines(header,new_fasta_file)
#  writeLines(sequence,new_fasta_file)
#}
#close(new_fasta_file)
