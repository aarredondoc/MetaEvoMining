#' @title make central DB
#' @description This function selects the protein sequences that are present in
#' more than half of the genomes in the pangenomic matrix. It uses the sequence
#' files obtained from Get_homologues and concatenates them into a FASTA file
#' with the appropriate headers for use in Evomining.
#' @usage search_shell_enzymes_DB<-function(csv_matrix,path)
#' @param csv_matrix is a pangenome matrix that results from
#' [get_homologs](http://eead-csic-compbio.github.io/get_homologues/manual/)
#' @param path is the output directory of alg_intersection of get_homologs
#' @details This function has a input  file with sequences and a list of file
#' names, and returns the first line of each file in the list concatenated into
#' a new file.It returns this header # >ID|1|accA_2|Alcanivoraxc15LHAGBDID_02122
#' @import readr dplyr stringi
#' @examples
#' \dontrun{
#' search_shell_enzymes_DB("pangenome_matrix_t0.tr.csv",path2)
#' }
#' @noRd


search_shell_enzymes_DB<-function(csv_matrix,
                                  path){

  # load the pangenome matrix and select count columns----------------------####

  Pangenome_matrix <- read_csv(paste0(path,csv_matrix))
  cols_to_keep <- grep("*.gbk", names(Pangenome_matrix), value= TRUE)
  matrix_subset <- Pangenome_matrix[cols_to_keep]
  row1 <- matrix_subset[1,]

  # select the genes which has copies in more than half --------------------####
  makerow_ofcondition <- function (row) {
    V <- as.logical(row)
    keepvalue <- sum(V)>length(V)/2
    return(keepvalue)
  }

  # apply the function to all rows and obtain a matrix subset---------------####

  allrow_values <- apply(matrix_subset,MARGIN = 1, makerow_ofcondition)
  matrix_subset$gene <- Pangenome_matrix$Gene
  matrix_subset$more_than_half <- allrow_values
  matrix_subset <- filter(matrix_subset,.data$more_than_half == TRUE)
  shell_genes <- matrix_subset$gene


  # this function has a input file a list of file names and returns the first
  # line of each file-------------------------------------------------------####

  filter_files <- function(file,lista,path) {
    file_path <- file.path(paste0(getwd(),"/",path,file,collapse = '|'))
    if (file %in% lista) {
      x <- readLines(file_path)
      sequence <- x[2]
      first_seq <- c(x[1],x[2])
      header <- first_seq[[1]][1]
      fragments <- stri_split(as.character(header), fixed = "|",simplify = TRUE)
      specie <- gsub("\\[|\\]","",fragments[2])
      header <- paste(
        stri_split(fragments[1],fixed = ":",simplify = TRUE)[1],
        "1", gsub(" ","_",fragments[5]),
        paste0(gsub(" ","",specie),gsub(" ",
                                        "",
                                        stri_split(fragments[1],
                                                   fixed = ":",
                                                   simplify = TRUE)[2]
                                        )
               ),
        sep="|")
      format<-c(header,sequence)
      return(format)
    }
  }


  fasta_lines <- lapply(shell_genes, function(file) {
    enzyme <- filter_files(file,shell_genes,path)
    cat(paste(enzyme[1],
              "\n",
              gsub(" ","",gsub("(.{80})", "\\1\n", enzyme[2], perl=TRUE)),
              collapse = "",sep = ""),"\n",
        file = .data$new_fasta_file, append = TRUE,sep = "")
  })


  # filter out the blank lines----------------------------------------------####
  non_blank_lines <- fasta_lines[!grepl("^\\s*$", fasta_lines)]

  fasta_lines <- non_blank_lines

  # initialize a counter for the sequence headers---------------------------####
  header_counter <- 1

  # loop over the lines and modify the sequence headers---------------------####
  for (i in 1:length(fasta_lines)) {
    if (startsWith(fasta_lines[i], ">")) {
      # extract the unique identifier from the header-----------------------####
      header_id <- gsub(">[^ ]+ ", "", fasta_lines[i], perl=TRUE)

      # construct the new header with the appended number-------------------####
      split<-stri_split(header_id, fixed = "|",simplify = TRUE)
      new_header <- paste(split[1],header_counter,split[3],split[4],sep="|")

      # replace the old header with the new one-----------------------------####
      fasta_lines[i] <- new_header

      # increment the header counter----------------------------------------####
      header_counter <- header_counter + 1
    }
  }
  if (fasta_lines[length(fasta_lines)] == "") {
    fasta_lines <- fasta_lines[-length(fasta_lines)]
  }

  # remove any trailing whitespace from the last line-----------------------####
  fasta_lines[length(fasta_lines)] <- gsub("\\s+$", "",
                                           fasta_lines[length(fasta_lines)])

  # write the modified character vector back to a new FASTA file------------####
  writeLines(fasta_lines, "Shell_DB.fasta")
}

