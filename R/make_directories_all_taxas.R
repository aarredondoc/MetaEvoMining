#' @title make_directories_by_family
#' @description This function uses group_files_by_taxa in a list of families
#' @usage make_directories_all_taxas(list_of_families,inputdir)
#' @param list_of_families is a vector that contains a list, taken out from a
#' column of taxonomy_table.
#' @param inputdir is the input file directory
#' @details This function uses lapply to make all the family directories with
#' group_files_by_taxa function.
#' @import readr dplyr
#' @examples make_directories_all_taxas(list_of_families,taxonomy_table)
#' @noRd

make_directories_all_taxas<-function(list_of_families,inputdir){

  output<-lapply(list_of_families,function(x)make_directories_by_taxa(inputdir,
                                                                 x))
  if (!is.null(output)) {
    print(message("done"))
  }
}

