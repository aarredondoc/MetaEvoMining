#' @title Make make dataframe for file
#' @description This function takes a protein FASTA file obtained from Prodigal,
#' an annotation ID table, the result of the allrast_names_table function,
#' and returns a dataframe with ID, coordinates 1 and 2, and amino acid
#' sequences.
#' @usage make_dataframeforfile(IDs_table,
#'                                  file_fasta,
#'                                          id)
#' @param IDs_table is a table of IDs made by make_allnamestable function
#' @param file_fasta is a path to a FASTA protein file.
#' @param id is an id of a sequence of the file.
#' @details This function reads a file that contains a list of sequence IDs
#' created by the make_Idlist function. It finds the complete line that matches
#' the second identifier, extracts only the ID from that line, replaces any "-"
#' with "_", removes the ">" symbol from the complete ID, finds the index of the
#' second identifier, and retrieves the amino acid sequence. It then finds the
#' start and stop coordinates and calculates the "strand" column by subtracting
#' the stop coordinate from the start coordinate and adding a "+" or "-" sign to
#' indicate the direction of the sequence. The function also adds the complete
#' ID up to the number after "scaffold" to the "contig_id" column, calculates
#' the species based on the ID, and outputs the results in an empty dataframe.
#' Finally, it fills in the rows of the dataframe with the relevant information.
#' @import readr dplyr stringi
#' @examples make_dataframeforfile(taxonomy_table,
#'           "inst/extdata/Proteome_named_scaff/5mSIPHEX1_18.faa",
#'                                          ">5mSIPHEX1_0-scaffold_1104_c1_1")
#' @noRd


make_dataframeforfile <- function(IDs_table,
                                  file_fasta,
                                  id){
  # Load table and multifasta file --------------------------------------####
  rast_ids <- IDs_table
  multi_fasta <- file_fasta
  #id<-paste0( id, "\\b")
  # list a sequence IDs of the file with make_Idlist---------------------####
  #listof_ids<- make_Idlist(multi_fasta)

  # prepare id
  #id<-paste(id_space, " ", sep = "")

  # find full lineof id--------------------------------------------------####
  grep_line <- stri_detect_fixed(multi_fasta,I(id))
  index_line<-which(grep_line==TRUE)
  full_line<-multi_fasta[index_line]
  # Cut the ID only -----------------------------------------------------####
  Id_split <-strsplit(full_line, "#")
  ID_only<-Id_split[[1]][1]

  # correct the name ----------------------------------------------------####
  id_completo<-gsub("-", "_", ID_only)
  id_mod<-paste(gsub('>', '',id_completo))

  # find index of the id ------------------------------------------------####
  grep_index <-grep(ID_only,multi_fasta, fixed = TRUE)

  # find aminoacid sequence ---------------------------------------------####
  aminoacid_sec <- multi_fasta[grep_index+1][1]
  aminoacid<-gsub('[*]', '',aminoacid_sec)

  # find coordinates ----------------------------------------------------####
  element <-strsplit(full_line, "#")
  #element[[1]][2]
  coordenada1 <- as.integer( element[[1]][2])
  #element[[1]][3]
  coordenada2 <- as.integer( element[[1]][3])

  # Calculates "strand" column-------------------------------------------####
  resta_dir <- coordenada2 - coordenada1
  strand <- as.character(ifelse(resta_dir < 0, "-", "+"))
  #add contig_id
  contig <-strsplit(id_mod, "_")
  contig_id<-contig[[1]][4]

  # Make feature id -----------------------------------------------------####
  identif_g<-strsplit(gsub("-","_",id),"_")
  ig<-gsub(">","",gsub(" ","_",paste(identif_g[[1]][1],identif_g[[1]][2])))
  x<-rast_ids[rast_ids$user_genome == ig,]
  #xvalue<-which(listof_ids == id, arr.ind = TRUE)
  #index_line <- stri_detect_fixed(multi_fasta,id)
  #x_row<-which(index_line==TRUE)

  feature<-paste("fig|",x$feature_id,".peg.",sep = "")

  # find specie ---------------------------------------------------------####
  specie<-lapply(x$gtdbk,`[[`, 1)
  speciesin_<-paste(gsub("_"," ",specie))

  # Make an empty dataframe ---------------------------------------------####
  df <- data.frame(matrix(ncol = 13, nrow = 0))
  colnames(df) <-c("contig_id",	"feature_id",	"type",	"location", "start",
                   "stop", "strand",	"locus_tag",	"figfam",	"species",
                   "nucleotide_sequence",	"amino_acid",	"sequence_accession")

  # fill the df with the columns ----------------------------------------####
  df[1,] <-c(contig_id,	feature,	"type",	"location",
             coordenada1, coordenada2, strand,	"unknown",
             "figfam",	speciesin_,	"nuc",	aminoacid, gsub(" ","",id_mod))

  return (df)

}
