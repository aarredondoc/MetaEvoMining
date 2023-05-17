#' @title Make taxonomy id
#' @description This function reads the gtdb-tk taxonomic mapping summary file
#' and returns a four-column file containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @usage make_taxonomy_id(table,id)
#' @param table is a dataframe of the report of [gtdbtk]
#' (https://github.com/Ecogenomics/GTDBTk.git) readed by read_tsv.
#' @param id is the name of ono of the user's genome.
#' @details This function get a dataframe of a GTDB-Tk file and returns a table
#' with four columns: two evomining IDs, the user's genome name, and taxonomy
#' assignation which can be incorporated into the taxonomy table.
#' @return a four-column table containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @import readr dplyr
#' @examples make_taxonomy_id(gtdbK_file,"5mSIPHEX2-concot_16")
#' @noRd


# This function  search for the identifiers--------------------------------####
make_taxonomy_id <- function(table,id){
  # search the row of id in gtdbk_file user_genome column------------------####
  x<-table[table$user_genome == id,]

  # find the row on the gtdbk that corresponds to the classification-------####
  w<-strsplit(x$classification, ";")

  # find the specie assigned or genus or family----------------------------####
  specie<-lapply(w,`[[`, 7)
  f<-specie
  if(specie=="s__") {
    f<-lapply(w,`[[`, 6)
    if(f=="g__"){
      f<-lapply(w,`[[`, 5)
    }
  }

  #cut specie and separate by __-------------------------------------------####
  s<-strsplit(as.character(f), "__")
  sp<-s[[1]][2]
  g_sp<- gsub(" ", "_", sp)

  #change the - for _ on id wich are in user_genome------------------------####
  id_completo<-gsub("-", "_",x$user_genome)

  # correct the ID---------------------------------------------------------####
  id_split <-strsplit(id_completo, "_")
  bin_id<-paste(id_split[[1]][1],"_",id_split[[1]][3],sep = "")

  # Add bin_id to the name of specie to differentiate----------------------####
  sp_rast<-paste(g_sp,bin_id,sep = "")

  # Assign a munber id based on index--------------------------------------####
  xvalue<-which(table == id, arr.ind = TRUE)
  x_row<-xvalue[1]
  row_size<- nchar(x_row)
  if(nchar(x_row) == 1 ){
    row_size<- as.numeric(x_row)*100000
  } else {
    if(nchar(x_row) == 2 ){
      row_size<- as.numeric(x_row)*11000
    }
  }
  value_id<-format(row_size, scientific = FALSE)
  feature_id<-paste("666666.",value_id,sep = "")

  fam<-lapply(w,`[[`, 5)

  # Make an empty dataframe and fill it -----------------------------------####
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df) <-c("id_numero",	"feature_id","user_genome","gtdbk","family")
  df[1,] <-c( value_id, feature_id, bin_id,	sp_rast,fam)
  return (df)
}
