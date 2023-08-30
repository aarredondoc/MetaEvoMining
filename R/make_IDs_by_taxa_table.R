#' @title write a table of annotation IDs by lineage
#' @description this function takes the gtdb-tk file and searches the MAGs that
#' belongs to the same taxonomic lineage and write a table with 3 columns:
#' number id, featured id and specie.
#' @usage make_IDs_by_taxa_table(fam,gtdbK_report)
#' @param gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @details This function makes the lineage_bins.IDs file needed to run
#' EvoMinining and is part of the MODULE1 of MetaEvoMining package.
#' 2023_03_15
#' @import readr
#' @importFrom utils write.table
#' @examples make_IDs_by_taxa_table("f__Alcanivoracaceae",taxonomy_table)
#' @noRd
make_IDs_by_taxa_table <- function(fam,
                                  gtdbK_report){
  # search for the same-lineage genomes-------------------------------------####
  df_filtered <- gtdbK_report[grepl(fam, gtdbK_report$family), ]

  # select the columns of taxonomy_table------------------------------------####
  family_id <- select(df_filtered, "id_numero", "feature_id", "gtdbk")
  family_id$gtdbk <- gsub("_"," ",family_id$gtdbk)

  # this code makes the name of the output table-----------------------------####
  fam_nm <- strsplit(fam,"__")
  fam_name <- fam_nm[[1]][2]

  write.table(family_id ,
              file =  paste("data/",fam_name,"_bins.IDs", sep = ""),
              sep = "\t",
              dec = ".",
              row.names = FALSE, col.names = FALSE, quote=FALSE)
}
