make_IDs_by_taxa_table_related <- function(fam,
                                   gtdbK_report){
  # search for the same-lineage genomes-------------------------------------####
  df_filtered <- gtdbK_report[grepl(fam, gtdbK_report$family), ]

  # select the columns of taxonomy_table------------------------------------####
  #family_id <- select(df_filtered, "id_numero", "feature_id", "gtdbk")
  #family_id$gtdbk <- gsub("_"," ",family_id$gtdbk)
  return(df_filtered)
  # this code makes the name of the output table-----------------------------####
  #fam_nm <- strsplit(fam,"__")
  #fam_name <- fam_nm[[1]][2]

  #write.table(family_id ,
   #           file =  paste("data/",fam_name,"_bins.IDs", sep = ""),
    #          sep = "\t",
     #         dec = ".",
      #        row.names = FALSE, col.names = FALSE, quote=FALSE)
}

taxatableby_lineage<-make_IDs_by_taxa_table_related("f__Alcanivoracaceae",taxtablerelated)

mi_lista_de_genomas <- paste(taxatableby_lineage$related_genomes,collapse = ",")

writeLines(mi_lista_de_genomas, con = "family_related_genomes.txt")

