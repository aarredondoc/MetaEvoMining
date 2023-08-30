#' @title Make taxonomy id
#' @description This function reads the gtdb-tk taxonomic mapping summary file
#' and returns a four-column vector containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @usage make_taxonomy_id(table,assembly_name)
#' @param table is a dataframe of the report of [gtdbtk]
#' (https://github.com/Ecogenomics/GTDBTk.git) readed by read_tsv.
#' @param assembly_name is the name of one of the user's genome.
#' @details This function get a dataframe of a GTDB-Tk file and returns a table
#' with four columns: two evomining IDs, the user's genome name, and taxonomy
#' assignation which can be incorporated into the taxonomy table.
#' @return a four-column table containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @import readr dplyr
#' @examples make_taxonomy_id(gtdbK_file,"5mSIPHEX2-concot_16")
#' @noRd

make_taxonomy_id <- function(table,
                             assembly_name){
  # search the row of id in gtdbk_file user_genome column-------------------####
  assembly_name_index<-table[table$user_genome == assembly_name,]

  # find the row on the gtdbk that corresponds to the classification--------####
  assembly_classification<-strsplit(assembly_name_index$classification, ";")

  # find the specie assigned or genus or family-----------------------------####
  specie<-lapply(assembly_classification,`[[`, 7)
  lineage<-specie
  if(specie == "s__"){
    lineage<-lapply(assembly_classification,`[[`, 6)
    if(f=="g__"){
      lineage<-lapply(assembly_classification,`[[`, 5)
    }
  }

  # cut specie and separate by __-------------------------------------------####
  s<-strsplit(as.character(lineage), "__")
  sp<-s[[1]][2]
  g_sp<- gsub(" ", "_", sp)

  # change the - for _ on id which are in user_genome-----------------------####
  id_completo<-gsub("-", "_", assembly_name_index$user_genome)

  # correct the ID----------------------------------------------------------####
  id_split <-strsplit(id_completo, "_")
  bin_id<-paste(id_split[[1]][1], "_", id_split[[1]][3], sep = "")

  # add bin_id to the name of specie to differentiate-----------------------####
  sp_rast<-paste(g_sp,bin_id,sep = "")

  # assign a number id based on index--------------------------------------####
  xvalue<-which(table == assembly_name, arr.ind = TRUE)
  x_row<-xvalue[1]
  row_size<- nchar(x_row)
  if(nchar(x_row) == 1){
    row_size<- as.numeric(x_row)*100000
  } else {
    if(nchar(x_row) == 2){
      row_size<- as.numeric(x_row)*11000
    }
  }
  value_id<-format(row_size, scientific = FALSE)
  feature_id<-paste("666666.",value_id,sep = "")

  fam<-lapply(assembly_classification,`[[`, 5)

  # make a row of taxonomy table--------------------------------------------####
  taxonomy_table_row <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(taxonomy_table_row) <- c("id_numero",
                                    "feature_id",
                                    "user_genome",
                                    "gtdbk",
                                    "family")

  taxonomy_table_row[1,] <- c(value_id, feature_id, bin_id,	sp_rast,fam)
  return (taxonomy_table_row)
}
