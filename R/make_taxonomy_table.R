#' @title Make taxonomy table
#' @description This function reads the gtdb-tk taxonomic mapping summary file
#' and returns a four-column file containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @usage make_taxonomy_table(gtdbK_report)
#' @param gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @details This function is part of the module 1 of MetaEvoMining package. This
#' function reads a GTDB-Tk file and returns a file with four columns: a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.This can
#' be incorporated into the make_EvoFiles pipeline to make a functional table.
#' @return a four-column file containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @import readr dplyr purrr
#' @examples make_taxonomy_table("inst/extdada/gtdbtk.bac120.summary.tsv")
#' @noRd

make_taxonomy_table<-function(gtdbK_report){

  # read the file-----------------------------------------------------------####
  gtdbK_file <- read_tsv(gtdbK_report)


  # make the taxonomy_table ------------------------------------------------####
  userg_list<-gtdbK_file$user_genome


  taxonomy_table <- map_df(userg_list, ~ make_taxonomy_id(gtdbK_file, .))


  return(taxonomy_table)
}
