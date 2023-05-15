#' @title Make taxonomy table names
#' @description This function reads the gtdb-tk taxonomic mapping summary file
#' and returns a four-column file containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @usage make_taxonomy_table(gtdbK_report)
#' @param gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @details This function is part of the module 2 of MetaEvoMining package. This
#' function reads a GTDB-Tk file and returns a file with four columns: the RAST
#' ID plus the user's genome name, which can be incorporated into the EvoMining
#' dataframe.
#' @return a four-column file containing a 6-digit numeric name, a unique
#' identifier, the user-assigned genome name, and the taxonomic mapping.
#' @import readr dplyr
#' @examples make_taxonomy_table("data/gtdbtk.bac120.summary.tsv")
#' @export

make_taxonomy_table<-function(gtdbK_report){

# Read the file------------------------------------------------------------####
  gtdbK_file <- read_tsv(gtdbK_report)


#make_taxonomy_id()
# apply to a list of IDs --------------------------------------------------####
userg_list<-gtdbK_file$user_genome

taxonomy_table<- plyr::ldply(.data =userg_list,
                             .fun= function(x) make_taxonomy_id(gtdbK_file,x),
                             .inform = FALSE)

# Write the file with all names and IDs------------------------------------####

#write.table(taxon_assig , file =  "data/taxonomy_table.IDs", sep = "\t",
#            dec = ".", row.names = FALSE, col.names = FALSE, quote=FALSE)
return(taxonomy_table)
}


