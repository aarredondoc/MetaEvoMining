library(dplyr)
library(readr)
library(stringi)
setwd("C:/Users/betterlab/Desktop/git_andres/00.get_homologs_25subset")

#path<-"Alcanivoracaceae/alg_intersection/"
path2<-"Flavobacteriaceae/alg_intersection/"

#example_gene<- "1415_accA_2.faa"
#listgenes<- c("1415_accA_2.faa","52012_nudJ.faa")

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
  new_fasta_file <- file("new3.fasta", "w")
  #enzyme<-filter_files(example_gene,shell_genes,path)
  # writeLines(enzyme,new_fasta_file, sep = "\n")
  #writeLines(sequence,new_fasta_file)
  #close(new_fasta_file)

  lapply(shell_genes, function(file) {
    enzyme<-filter_files(file,shell_genes,path)
    cat(paste(enzyme[1], "\n", gsub(" ","",gsub("(.{80})", "\\1\n", enzyme[2], perl=TRUE)), collapse = "",sep = ""),"\n", file = new_fasta_file, append = TRUE,sep = "")
  })

  close(new_fasta_file)


  fasta_lines <- readLines("new3.fasta")

  # filter out the blank lines
  non_blank_lines <- fasta_lines[!grepl("^\\s*$", fasta_lines)]

  # write the non-blank lines back to the file
  writeLines(non_blank_lines, "multiline_DB.fasta")

  fasta_lines <- readLines("multiline_DB.fasta")

  # initialize a counter for the sequence headers
  header_counter <- 1

  # loop over the lines and modify the sequence headers
  for (i in 1:length(fasta_lines)) {
    if (startsWith(fasta_lines[i], ">")) {
      # this line is a sequence header
      # extract the unique identifier from the header
      header_id <- gsub(">[^ ]+ ", "", fasta_lines[i], perl=TRUE)
      # construct the new header with the appended number
      split<-stri_split(header_id, fixed = "|",simplify = TRUE)
      new_header <- paste(split[1],header_counter,split[3],split[4],sep="|")
      # replace the old header with the new one
      fasta_lines[i] <- new_header
      # increment the header counter
      header_counter <- header_counter + 1
    }
  }
  if (fasta_lines[length(fasta_lines)] == "") {
    fasta_lines <- fasta_lines[-length(fasta_lines)]
  }

  # remove any trailing whitespace from the last line
  fasta_lines[length(fasta_lines)] <- gsub("\\s+$", "", fasta_lines[length(fasta_lines)])
  # write the modified character vector back to a new FASTA file
  writeLines(fasta_lines, "FlavobacteriaceaeDB.fasta")
}

search_shell_enzymes_DB("pangenome_matrix_t0.tr.csv",path2)

#fasta_lines <- readLines("new2.fasta")

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
