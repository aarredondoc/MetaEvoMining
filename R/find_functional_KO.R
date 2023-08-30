#' @title Find a metabolic annotation for the sequence
#' @description This function takes an ID as input and returns the KO metabolic
#' function associated with it.
#' @usage find_metabolic_function(scaffold_id,KO_report)
#' @param id is an id of each protein sequence in the file
#' @param KO_report is a dataframe given by read_ko function that reads a KO
#' file of all sequences of the genome created with
#' [KofamScan](https://github.com/takaram/kofam_scan.git).
#' @details Given an ID, this function returns returns a dataframe containing
#' the ID and associated metabolic function and Add a '>' character to the
#' beginning of the ID.
#' @import readr
#' @examples find_metabolic_function("5mSIPHEX1_0-scaffold_1104_c1_2", k0_file)
#' @noRd

find_metabolic_function <-function(scaffold_id,KO_report){
  # search the id----------------------------------------------------------####
  grep_id <-KO_report[scaffold_id==KO_report$Scaffold_name, ]

  # returns a dataframe with the first metabolic function-------------------####
  grep_id[1,3]
  metabolic<-  grep_id[1,3]

  # adds a > to the id-------------------------------------------------------####
  dataframe <- data.frame(unique(scaffold_id),metabolic)

  return(dataframe)
}

