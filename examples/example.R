filter_userID<-function(data_gtdbtk, data_list_names=NULL, data_names=NULL){
  #Variables------------------------------------------------------------------####
  gtdbK_file <- read_tsv(data_gtdbtk)
  return(gtdbK_file)
}
library("readr")
filter_userID("data/gtdbtk.bac120.summary.tsv")
