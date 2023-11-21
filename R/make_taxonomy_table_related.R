make_taxonomy_table_related<-function(gtdbK_report){

  # read the file-----------------------------------------------------------####
  gtdbK_file <- read_tsv(gtdbK_report)


  # make the taxonomy_table ------------------------------------------------####
  userg_list<-gtdbK_file$user_genome


  taxonomy_table <- map_df(userg_list, ~ make_taxonomy_id_Related(gtdbK_file, .))


  return(taxonomy_table)
}


#library(dplyr)
#library(purrr)
#library(readr)
#taxtablerelated<-make_taxonomy_table_related("inst/extdata/gtdbtk.bac120.summary.tsv")
