#' @title Make ID list
#' @description This function extracts sequence IDs from a protein FASTA file
#' obtained from Prodigal.
#' @usage make_Idlist(file)
#' @param file is the character vector when opens with readLines protein fasta
#' obtained from [Prodigal](https://github.com/hyattpd/Prodigal.git)
#' @details This function is part of the MetaEvoMining package. We identified
#' all lines that contain '>', requested those lines from the file, cut them by
#' the '#' character, and for each selection chose the first line.
#' @return a list of all sequence IDs in the file
#' @examples make_Idlist(file)
#' @noRd
#'
# read file -------------------------------------------------------------#####

make_Idlist <- function(file){

  # cleaning names from ids ------------------------------------------------####
  grep_index <- grep(">",file)
  ID_lines <- file[grep_index]
  Id <- strsplit(ID_lines, "#")
  # list all ---------------------------------------------------------------####
  IDs <- sapply(Id, `[`, 1)
  return (IDs)
}

