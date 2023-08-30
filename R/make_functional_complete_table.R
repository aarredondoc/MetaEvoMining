#' @title Make a complete files with annotation for evomining
#' @description This function takes as input the directory containing proteome
#' sequences obtained from Prodigal, the directory containing KO outputs, and
#' the result file from the allrast_names_table function. It returns an
#' annotated file with species information, as well as a FASTA file containing
#' simulated RAST feature IDs.
#' @usage make_complete_files(file_path, IDs_table, annotation_dirpath)
#' @param file_path is a path to a FASTA protein file.
#' @param IDs_table is a table of IDs made by make_taxonomy_table function.
#' @param annotation_dirpath is the path of the annotation output directory
#' created with [KofamScan](https://github.com/takaram/kofam_scan.git).
#' @details Add the "Function" column to the dataframe resulting from
#' make_dataframeforfile function, and merge it with the dataframe resulting
#' from the find_metabolic_function function. The merge should be done between
#' the two dataframes using a common column. Write two files, one is a table and
#' the other one is a fasta with special ids.
#' @import readr dplyr purrr
#' @importFrom utils write.table
#' @examples
#'make_complete_files("inst/extdata/Proteome_named_scaff/5mSIPHEX1_1.faa",
#'                                                         taxonomy_table,
#'                                               "inst/extdata/KO_output")
#' @noRd


make_complete_files <- function(file_path,
                              IDs_table,
                              annotation_dirpath) {
  # load table and multifasta file -----------------------------------------####
  multifasta_file <- readLines(file_path)
  rast_ids<-IDs_table

  # This code makes a list of all sequence ids------------------------------####
  listof_ids <- I(as.list(make_Idlist(multifasta_file)))

  # this code make the full dataframe---------------------------------------####

  df_1235 <- map_dfr(listof_ids,
                     ~ make_dataframeforfile(rast_ids, multifasta_file, .))



  # this code reads the KEGG file-------------------------------------------####

  n < -strsplit(listof_ids[[1]][1],"-")[[1]][1]
  w <- gsub(">","",n)
  k0_file <- make_functional_KO_dataframe(paste(annotation_dirpath,
                                              w,
                                              ".faa.txt"
                                              ,sep = ""))

  # this code clean the ids--- ---------------------------------------------####
  Lista_IDs <- gsub(" ","",gsub(">","",gsub("-scaffold","_scaffold",listof_ids)))

  # this code makes a table with functional annotation per sequence---------####

  ko_df <- map_dfr(Lista_IDs, ~ find_metabolic_function(.x,
                                                        k0_file))

  # change the name of the column function ---------------------------------####
  colnames(ko_df)[2] <- "function"

  # this code merge ko data with the full functional table------------------####

  df_final = merge(x = df_1235 , y = ko_df, by.y = 1, by.x=13, all.x = TRUE)

  dat_2 <- select(df_final,"contig_id",	"feature_id",	"type",	"location",
                  "start", "stop", "strand","function",	"locus_tag",	"figfam",
                  "species",	"nucleotide_sequence",	"amino_acid",
                  "sequence_accession")

  dat_2$feature_id <- paste(dat_2$feature_id,seq(1, nrow(dat_2)),sep = "")

  # write the full functionl table------------------------------------------####
  name <- rast_ids[rast_ids$user_genome == w,]
  id_num <- name$id_numero
  rast_able <- write.table(dat_2, paste("data/",id_num,".txt",sep = ""),
                         append = F,
                         sep = '\t',
                         dec = ".",
                         row.names = FALSE,
                         col.names = TRUE,
                         quote=FALSE)

  # write the protein fasta file--------------------------------------------####
  Xfasta <- character(nrow(dat_2) * 2)
  Xfasta[c(TRUE, FALSE)] <- paste(">", dat_2$feature_id, sep = "")
  Xfasta[c(FALSE, TRUE)] <- dat_2$amino_acid
  file_fasta <- writeLines(Xfasta, paste("data/",id_num,".faa",sep = ""))


  return(message("Done."))

}
