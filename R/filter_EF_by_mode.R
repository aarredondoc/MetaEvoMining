#' @title Filter above mode
#' @description This function searches the EvoMining table and looks for columns
#' (enzymes) where the counts in the input genomes are above the mode.
#' @usage filter_EF_above_mode(f,g_names)
#' @param f a vector that contains a column
#' @param g_names a is a vector that contains genome names
#' @details returns true or false if the genomes are in the column above the
#' mode or not
#' @import dplyr readr
#' @examples filter_EF_above_mode(EF2,Mydata_bins)
#' @return true or false if the genomes are in the column above the mode
#' @noRd

filter_EF_above_mode <- function(f,
                                 g_names){
  EFmode <- getmoda(f)
  abovemodeindexes <- which(f > EFmode)
  binsabovemode <- g_names[which(g_names%in%abovemodeindexes)]
  trueorfalse <- any(binsabovemode)
  return(trueorfalse)

}
