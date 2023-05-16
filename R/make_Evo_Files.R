#' @title Make EvoMining files
#' @description Esta funcion toma El directorio de secuencias de Proteomas
#' obtenidas de prodigal, el directorio de los outputs de KO y el archivo
#' resultado de la funcion allrast_names_table y regresa un archivo anotado con
#' especie y un fasta con los feature ids de rast simulados
#' @usage make_EvoFiles(annotation_dirpath,
#'                          genome_dirpath,
#'                              IDs_table)
#' @param annotation_dirpath is the path of annotation output directory created
#' by [KofamScan](https://github.com/takaram/kofam_scan.git).
#' @param genome_dirpath is the path of all protein sequences files of all the
#' bins
#' @param gtdbK_report gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @details This function is part of the MetaEvoMining package
#' @import readr dplyr
#' @return a list of all sequence IDs in the file
#' @examples make_EvoFiles("Datos/02.KO_results_all/",
#'                         'data/03/03.Proteome_named_scaff/',
#'                         "data/gtdbtk.bac120.summary.tsv")
#' @export

make_EvoFiles <- function(gtdbK_report,
                          genome_dirpath,
                          annotation_dirpath){

  # Make all namestable ----------------------------------------------------####
  # Merge genome names and taxonomy from gdtbtk ----------------------------####

  #allnames_table<-make_allnames_table(gtdbK_report)
  taxonomy_table<-make_taxonomy_table(gtdbK_report)
  # make_taxonomy_table

  # Make all the files for Evomining for a directory------------------------####
  # Merge taxonomy table and functional annotation -------------------------####

  dir <- genome_dirpath
  allFiles <- list.files(dir)
  # que j no se llame j, dir_allFiles
  dir_allFiles<-c(paste(dir,allFiles,sep = ""))
   #list<-c("data/03/03.Proteome_named_scaff/700mSIPHEX2_24.faa",
      #    "data/03/03.Proteome_named_scaff/700mSIPHEX2_9.faa")

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

make_EvoFiles("data/gtdbtk.bac120.summary.tsv",
              "data/03/03.Proteome_named_scaff/",
              "data/02.KO_results_all/")


#gtdbK_report == "data/gtdbtk.bac120.summary.tsv"

#annotation_dirpath== "data/02.KO_results_all/"
#genome_dirpath =="data/03/03.Proteome_named_scaff/"
