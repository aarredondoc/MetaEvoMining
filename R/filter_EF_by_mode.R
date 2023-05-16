#' @title Filter above mode
#' @description This function searches the EvoMining table and looks for columns
#' (enzymes) where the counts in the input genomes are above the mode.
#' @usage filter_EF_above_mode(f,g_names)
#' @param f a vector that contains a column
#' @param g_names a is a vector that contains genome names
#' @details returtrue or false if the genomes are in the column above the mode
#' @import dplyr rlang readr plyr
#' @examples filter_EF_above_mode(EF2,Mydata_bins)
#' @return true or false if the genomes are in the column above the mode
#' @export

##above_mode_fun------------------------------------------------------------####

#input a vector that contains a column
#input2 genomas de interes
#output verdadero o falso si los genomas se encuentran en la columna por arriba
#de la moda
filter_EF_above_mode <- function(f,g_names){#f is a vector and df is a dataframe
  EFmode <- getmoda(f)
  abovemodeindexes <- which(f > EFmode)
  binsabovemode<-g_names[which(g_names%in%abovemodeindexes)]
  trueorfalse<-any(binsabovemode)
  return(trueorfalse)

}
