#' @title Make a dataframe with blast e-values genomesVsNP
#' @description Load the genomes vs. NP blast file and search for the five
#' better results, and the best e-value and make a mean and median of the five.
#' then makes a dataframe with the results.
#' @usage make_fam_np_evalue(blast_genomesvsNP, blast_genomesvsfam)
#' @param blast_genomesvsNP blast files of genomes versus Natural Products.
#' @param blast_genomesvsfam dataframe of blast file genomes versus central
#' families.
#' @details This function is used in make_evalue_df_for_plot funtion.
#' @import dplyr
#' @return A dataframe with 4 columns: Families, MeanNP, MedianNP, ClosestNP.
#' @noRd
#' @examples
#' genomeNPvalues<-lapply(archivosGenomesvsNP,
#'                         make_fam_np_evalue,
#'                          blast_genomesvsfam)
#'

make_fam_np_evalue <- function(blast_genomesvsNP,
                               blast_genomesvsfam) {

  #load files
  #blast_genomesvsfam<-read.table(blast_genomesvsFam)
  blast_genomesvsnp<-read.table(blast_genomesvsNP)

  # Encontrar las correspondencias de IDs entre df1 y df2-------------------####
  matching_indices <- match(blast_genomesvsnp$V1, blast_genomesvsfam$V2)

  # Agregar la columna Valor2 de df2 a df1 basado en las correspondencias
  blast_genomesvsnp$Familia <- blast_genomesvsfam$V1[matching_indices]
  #print(blast_genomesvsnp)

  # Omit NA
  blast_genomesvsnp<-na.omit(blast_genomesvsnp)

  # Order and slice to 5 and best e values----------------------------------####
  blast_genomesvsnp <- blast_genomesvsnp[order(blast_genomesvsnp$V11),]
  first5_np<-slice_head(n = 5, blast_genomesvsnp)
  first5_np<- first5_np[,c(2,11,13)]
  closestNP<-first5_np[1,]


  # Calculate mean and median of top 5
  mean_NP<-mean(first5_np$V11)
  median_NP<-median(first5_np$V11)

  # make a result datframe--------------------------------------------------####
  NP_evalue_dataframe <- data.frame(
    Familia = unique(first5_np$Familia),
    mediaNP = mean_NP,
    medianaNP = median_NP,
    closestNP = closestNP[,"V11"]
  )

}



