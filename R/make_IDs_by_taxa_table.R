#' @title make sorted IDs by family
#' @description Esta función toma el archivo gtdb-tk y busca los MAGs que
#' pertenecen a la misma clase taxonómica y regresa un archivo parecido al ids
#' de rast las filas son: nombre numérico, identificador único y especie
#' @usage make_IDs_by_taxa_table(gtdbK_report,taxa)
#' @param gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @details This function is part of the MODULE2 of MetaEvoMining package
#' 2023_03_15
#' @import readr dplyr
#' @examples make_IDs_by_taxa_table(taxonomy_table)
#' @noRd
make_IDs_by_taxa_table<- function(fam,gtdbK_report){

  df_filtered <- gtdbK_report[grepl(fam, gtdbK_report$family), ]

  family_id<-select(df_filtered, "id_numero", "feature_id", "gtdbk")

  family_id$gtdbk<-gsub("_"," ",family_id$gtdbk)

  fam_nm<-strsplit(fam,"__")
  fam_name <-fam_nm[[1]][2]

  write.table(family_id , file =  paste("data/",fam_name,"_bins.IDs", sep = ""), sep = "\t", dec = ".",
              row.names = FALSE, col.names = FALSE, quote=FALSE)
}
