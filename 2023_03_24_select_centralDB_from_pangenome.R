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


#libreria #stringi

# --------------------------------------------------------------------------####

#paste0(getwd(),"/",path,example_gene)
#setwd(path)
#file_path <- file.path(paste0(getwd(),"/",path,example_gene))
#file_path <- file.path(getwd(),example_gene)
#file_path
#file.info(file_path)$is.file
#all_files <- list.files(path)

#file_list <-list.files(path,pattern = "*.faa")
#file_list
#getwd()
#files_list <- list.files()
#results_list <- lapply(shell_genes,file_list)
##Tengo un archivo con secuencias y una lista de nombres de archivos
#"52003_glcB.faa"                   "52004_hypothetical_protein.faa"
##quiero la primera linea de cada archivo de la lista concatenado en un archivo nuevo
#>ID:LHAGBDID_03183 |[Alcanivorax c15]|strain|Alcanivorax_C15_015665355.prokka.gbk|hldD|2007|JADFAH010000003.1(107935):46959-48965:1 ^^ Alcanivorax c15 strain strain.|neighbours:ID:LHAGBDID_03182(-1),ID:LHAGBDID_03184(1)|neighbour_genes:fabR_2,betA|
#MNYFVTGATGFIGRFLVGRLLKRDDTRVFALVRCGSEYKLDALRNRLGVDADRLVAIHGDINEKLLGVSKRDQDDLNGQIDHFFHLAAIYDLTADENTQRYTNIEGTRQTLKLAEKIQAGRFHHVSSIAAAGLYDGTFTENMFEEATGLDDAYLLTKHESEALVRQESKIPWRIYRPAMVVGHSETGAMDKVDGPYYLFKFIQKLKDVLPNWIPLIGVEGGHFNVVPVDFVADALDHIAHQPDHDGQCFHLTADRSYSLGEIMDIVAGAAQAPRMALKLENRLFDVVPGFVRKGVSALTPNLLLNAALDNLDIPPSAMKFLTFPTEYDNSRAKAALEGTGIAAPELESYIQQLWDFWENHLDPDRGERKDELQPLPTLPERVEGKVVMVTGATSGIGKASALKLARAGATVLVIARTAEKLEETLHEIDQLGGTAQAYSCDVSDLTDVDNLVQQVLADHGHVDILVNNAGRSIRRSVVHAFDRFHDYERTMQLNYFGALRLIMQLMPSMIENGGGHVINISSIGVLTNAPRFSAYVASKAALDAFTRCASSELAHEGIRFTTINMPLVRTPMIAPTKIYNHVPTISPTQAADMICDAIVRQPKRIATNLGVMGQVMHFITPRVTETIMNTGYKLFSDSSAALGGKEGTPKKIRREQAAFSRLFKGIHW
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

shell_genes<- matrix_subset$gene
example_gene<- "1415_accA_2.faa"
listgenes<- c("1415_accA_2.faa","52012_nudJ.faa")
path<-"Alcanivoracaceae/alg_intersection/"
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
shell_genes<- matrix_subset$gene
example_gene<- "1415_accA_2.faa"
listgenes<- c("1415_accA_2.faa","52012_nudJ.faa")
path<-"Alcanivoracaceae/alg_intersection/"
enzyme<-filter_files(example_gene,shell_genes,path)
new_fasta_file <- file("new3.fasta", "w")
#enzyme<-filter_files(example_gene,shell_genes,path)
 # writeLines(enzyme,new_fasta_file, sep = "\n")
  #writeLines(sequence,new_fasta_file)
  #close(new_fasta_file)


  lapply(shell_genes, function(file) {
    enzyme<-filter_files(file,shell_genes,path)
    cat(paste(enzyme[1], "\n", gsub(" ","",gsub("(.{80})", "\\1\n", enzyme[2], perl=TRUE)), collapse = "",sep = ""),"\n", file = new_fasta_file, append = TRUE,sep = "")
    #cat(paste(enzyme, collapse = "\n"), "\n", file = new_fasta_file, append = TRUE)
  })
  #fasta_file <- paste(fasta_lines, collapse = "\n")
  #writeLines(fasta_file, "ple.fasta")

  close(new_fasta_file)

#  if (!requireNamespace("Biostrings", quietly = TRUE)) {
#    install.packages("Biostrings")
#  }
#  library(Biostrings)
#  uniline_fasta <- readDNAStringSet("Alcanivoracaceae_CentralDB", format = "fasta")
#  writeXStringSet(uniline_fasta, "new_multi_file.fasta", format = "fasta")

#filter_files(example_gene,shell_genes,path)
#file.create("output.txt")
#test<-lapply(listgenes, function(file) {
 # enzyme<-filter_files(file,shell_genes,path)
  #cat(paste(new_seq, collapse = "\n"), "\n", file = output_file, append = TRUE)
  #writeLines(enzyme,new_fasta_file, sep = "\n")
  #close(new_fasta_file)
#})


    #match <- stri_locate_first_regex(x, ">ID:")
    #print(first_seq)
    #first_seq = substr(1,x[match[1]])
    #if (!is.na(match[1])) {
    #  first_true <- substr(x[match[1]], match[2], match[3])
    #  print(paste("The first true value in the file is:", first_true))
    #} else {
    #  print("No true values found in the file")
    #}
    #} else {
    #  print("hi")
    #list(file = file, matches = NULL)

  # >ID|1|accA_2|Alcanivoraxc15LHAGBDID_02122

file_path <- file.path(paste0(getwd(),"/",path,example_gene,collapse = '|'))
if (example_gene %in% shell_genes) {
  x <- readLines(file_path)
  sequence<-x[2]
  first_seq <-c(x[1],x[2])
  header<-first_seq[[1]][1]
  fragments <- stri_split(as.character(header), fixed = "|",simplify = TRUE)
  specie<-gsub("\\[|\\]","",fragments[2])
  header<-paste(stri_split(fragments[1],fixed = ":",simplify = TRUE)[1],"1",fragments[5],paste0(gsub(" ","",specie),stri_split(fragments[1],fixed = ":",simplify = TRUE)[2]),sep="|")
  new_fasta_file <- file("new1.fasta", "w")
  writeLines(header,new_fasta_file)
  writeLines(sequence,new_fasta_file)
}
close(new_fasta_file)
