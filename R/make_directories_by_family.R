#' @title make_directories_by_family
#' @description Esta función toma el archivo gtdb-tk y busca los MAGs que
#' pertenecen a la misma clase taxonómica y regresa un archivo parecido al ids
#' de rast las filas son: nombre numérico, identificador único y especie
#' @usage make_directories_by_family(list_of_families,inputdir)
#' @param list_of_families is a column of taxonomy_table.
#' @param inputdir is the file directory
#' @details This function is part of the MODULE2 of MetaEvoMining package
#' 2023_03_15
#' @import readr dplyr
#' @examples make_taxonomyIDs_by_family(list_of_families,taxonomy_table)
#' @noRd

make_directories_by_family<-function(list_of_families,inputdir){

  #fileIDS <- list.files(inputdir,pattern = "\\.IDs$")

  output<-lapply(list_of_families,function(x)group_files_by_taxa(inputdir,
                                                                 x))
  if (!is.null(output)) {
    print(message("done"))
  }
}

