#' Make a plot with -log10 evalues diferencies between copies in Enzyme families
#' @description Make a dataframe with evalues diferencies converted to -10log()
#' between copies in Enzyme families and makes a plot.
#' @usage plot_evalue_differences(blastfile, directory)
#' @param blastfile blast file of genomesDBvsFamiliesDB
#' @param directory Path to GenomesvsNP blast's directory.
#'
#' @import ggplot2 tidyr scales
#'
#' @return a plot with median, and mean of diference in evalues in -log10 +
#' a constant between famvsNPs and genomesvsfams
#' @noRd
#'
#' @examples
#' plotmm1<- plot_evalue_differences("pscplos17.blast.uniq",".")

plot_evalue_differences<-function(blastfile, directory){

  # load blast file---------------------------------------------------------####
  blast_genomesvsfam<-read.table(blastfile)

  # shorten the dataframe
  matriz_resultante <- as.data.frame(blast_genomesvsfam[, c(1,2, 11)])
  matriz_resultante$V11<- as.numeric(matriz_resultante$V11)
  matriz_resultante <- matriz_resultante[order(matriz_resultante$V11),]

  # make a list of all families in the file
  Familieslist<-unique(matriz_resultante$V1)

  ## calculate evalues to NP------------------------------------------------####
  archivosGenomesvsNP <- as.list(list.files(directory,
                                            pattern = "*ExpandedVsNp.blast.2"))
  genomeNPvalues<-lapply(archivosGenomesvsNP,
                         make_fam_np_evalue,
                         blast_genomesvsfam)
  mm_NP_df <- do.call(rbind, genomeNPvalues)
  print(mm_NP_df)

  # calculate mean and median of the diferences between e-values------------####
  meanmedian_data<-lapply(Familieslist,
                          make_evalue_df_forplot,
                          matriz_resultante,
                          mm_NP_df )

  result_mm_df <- do.call(rbind, meanmedian_data)
  #print(result_mm_df)

  # format the data to see in the same plot---------------------------------####
  data_long_mm <- gather(result_mm_df,
                         key = "columna",
                         value = "valor",
                         -Familia)

  # make a plot to compare means and medians
  mmplot <- ggplot(data_long_mm, aes(x = Familia, y = valor, fill = columna)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    labs(title = "Comparación de e-valores en -log10",
         x = "Nombres",
         y = "Valor",
         fill = "Columna") +
    #scale_y_log10(oob = scales::squish_infinite) +  # Ajuste de escala en eje y
    theme_minimal() +  # Rotar etiquetas en eje x
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(mmplot)
}
