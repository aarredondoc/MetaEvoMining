#' @title Make EvoMining files
#' @description Esta funcion toma El directorio de secuencias de Proteomas
#' obtenidas de prodigal, el directorio de los outputs de KO y el archivo
#' resultado de la funcion allrast_names_table y regresa un archivo anotado con
#' especie y un fasta con los feature ids de rast simulados
#' @usage make_EvoFiles(gtdbK_report, genomes_dirpath, annotation_dirpath)
#' @param gtdbK_report gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @param genomes_dirpath is the path of all protein sequences files of all the
#' bins
#' @param annotation_dirpath is the path of annotation output directory created
#' by [KofamScan](https://github.com/takaram/kofam_scan.git).
#' @details This function is part of the MetaEvoMining package
#' @import readr dplyr
#' @return a list of all sequence IDs in the file
#' @examples
#' \dontrun{
#'  make_EvoFiles("inst/extdata/gtdbtk.bac120.summary.tsv",
#'                             "inst/extdata/Proteome_named_scaff/",
#'                                                "inst/KO_output/")
#'}
#' @export

make_EvoFiles <- function(gtdbK_report,
                          genomes_dirpath,
                          annotation_dirpath){


  # make_taxonomy_table-----------------------------------------------------####
  taxonomy_table<-make_taxonomy_table(gtdbK_report)


  # Make the funtion make_complete_files to all the input files ------------####

  dir <- genomes_dirpath
  allFiles <- list.files(dir)
  dir_allFiles<-c(paste(dir,allFiles,sep = ""))

  output<-lapply(dir_allFiles,
                 function(x) make_complete_files(x,
                                                 taxonomy_table,
                                                 annotation_dirpath))
  if (!is.null(output)) {
    print(message("done"))
    }

  # List all sample taxonomic families--------------------------------------####
  list_of_families<-c(sort(unique(taxonomy_table$family)))

  # create a functional annotation IDs file by family-----------------------####
  make_IDs_by_taxa_files(list_of_families,taxonomy_table)

  # Make separate directories by family-------------------------------------####
  make_directories_all_taxas(list_of_families,"data/")

  #create a new directory and move all results-----------------------------####
  path_results<-dir.create("data/Results")
  directories_toMove<-c(paste0("data/",
                               lapply(strsplit(list_of_families,"__"),`[[`, 2)))

  move_directories(directories_toMove,
                   "data/Results")

  # Remove the original directories

}
