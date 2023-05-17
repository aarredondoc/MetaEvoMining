#' @title Make functional annotation dataframe
#' @description This function reads the KO output and filter the most probably
#' KO functional annotations associated with it.
#' @usage make_functional_KO_dataframe(ko_dir)
#' @param id is an id of each protein sequence in the file
#' @param ko_dir is a directory of given KO file of one genome created with
#' [KofamScan](https://github.com/takaram/kofam_scan.git).
#' @details Given an ID, this function returns returns a dataframe containing
#' the ID and associated metabolic function and Add a '>' character to the
#' beginning of the ID.
#' @import dplyr purrr stringr tidyr
#' @examples
#' make_functional_KO_dataframe("inst/extdata/KO_output/5mSIPHEX1_0.faa.txt")
#' @noRd
suppressWarnings({
make_functional_KO_dataframe<-function(ko_dir){
files <- dir(path = ko_dir ,pattern ="*.txt")
final_files<-paste0(ko_dir, files)
table_Kofam<-
    final_files %>%
      map_dfr(read_table2, col_names = F) %>%
      filter(str_detect(.data$X1, '\\*')) %>%
      select(.data$X2,.data$X3) %>%
  dplyr::rename(Bin_name = .data$X2) %>%
  dplyr::rename(KO = .data$X3) %>%
  separate(.data$Bin_name, c("Bin_name", "Scaffold_name"),
           sep = "[_|-][s|S]caffold") %>%
  mutate(Scaffold_name = paste0("scaffold", .data$Scaffold_name),
         .data$Scaffold_name) %>%
  unite("Scaffold_name", c("Bin_name", "Scaffold_name"), remove = FALSE)
}
})
