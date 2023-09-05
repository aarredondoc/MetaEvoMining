#' Title
#'
#' @param Enzymefamily
#' @param matriz_resultante
#'
#' @return
#' @export
#'
#' @examples
make_evalue_df_forplot_byfamily <- function(Enzymefamily,matriz_resultante,mm_NP_df) {
  #Enzymefamily<-"3PGA_AMINOACIDS|1|Phosphoglycerate_dehydrogenase_1|Cglu"
  # subset dataframe by family and order it
  subconjunto <- matriz_resultante[matriz_resultante$numeros == Enzymefamily, ]
  subconjunto <- subconjunto[order(subconjunto$V11),]

  # calculate the differences between e-values
  diferencias <- abs(diff(subconjunto$V11))

  # calculate mean and median to compare them
  mean_dif<-mean(diferencias)
  median_dif<-median(diferencias)


  # make a dataframe with the results
  resultsdataframe <- data.frame(
    Familia = unique(subconjunto$numero),
    CENTRALmean= -log10(mean_dif+1e-200),
    CENTRALmedian =-log10(median_dif+1e-200)
    #closest5NPmedian = -log10(abs(median_dif - mm_NP_df$closest5NPmedian)+ 1e-200)
    #closest5NPmean = -log10(abs(mean_dif - mm_NP_df$closest5NPmean+ 1e-200)),
    #closestnpdif = -log10(abs(median_dif - mm_NP_df$closestNP)+ 1e-200)
    #media = if (mean_dif == 0) {
    #  0
    #} else {
    # -log10(mean_dif)
    #},
    #mediana = if (median_dif == 0) {
    #  0
    #} else {
    #  -log10(median_dif)
    #}
  )
  #print(resultsdataframe)
  #calculate differences NP
  mm_NP_df$closest5NPmedian <- -log10(abs(median_dif - mm_NP_df$closest5NPmedian))
  mm_NP_df$closestmean5NP <- -log10(abs(mean_dif - mm_NP_df$closestmean5NP))
  mm_NP_df$closestNP <- -log10(abs(median_dif - mm_NP_df$closestNP))
  #print(mm_NP_df)

  # Reemplazar la columna 'nombres' con los números
  mm_NP_df <- mm_NP_df %>%
    mutate(numeros = extraer_numero(Familia))

  # add cols by index
  match_indices <- match(resultsdataframe$Familia,mm_NP_df$numeros)
  #print(match_indices)
  # Agregar la columna Valor2 de df2 a df1 basado en las correspondencias
  resultsdataframe$closestmean5NP <- mm_NP_df$closestmean5NP[match_indices]
  resultsdataframe$closest5NPmedian <- mm_NP_df$closest5NPmedian[match_indices]
  resultsdataframe$closestNP <- mm_NP_df$closestNP[match_indices]
  # Omit NA
  #resultsdataframe<-na.omit(resultsdataframe)
  #print(resultsdataframe)


  return(resultsdataframe)
}
#closest5NPmedian = median_dif - mm_NP_df$closest5NPmedian
