#' @title Make rast IDs file by taxa
#' @description Esta función toma el archivo gtdb-tk y busca los MAGs que
#' pertenecen a la misma clase taxonómica y regresa un archivo parecido al ids
#' de rast las filas son: nombre numérico, identificador único y especie
#' @usage make_IDs_by_taxa_files(list_of_families,gtdbK_report)
#' @param gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @param list_of_families is a column of taxonomy_table.
#' @details This function is part of the MODULE2 of MetaEvoMining package
#' 2023_03_15
#' @import readr
#' @examples make_IDs_by_taxa_files(list_of_families,taxonomy_table)
#' @noRd

make_IDs_by_taxa_files<-function(list_of_families,
                                 gtdbK_report){


  output<-lapply(list_of_families,function(x)make_IDs_by_taxa_table(x,
                                                      gtdbK_report))
  if (!is.null(output)) {
    print(message("ID files created"))
    }
}



