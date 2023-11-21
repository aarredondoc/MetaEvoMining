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
#' @export
#'
#' @examples
#' plotmm1<- plot_evalue_differences("pscplos17.blast.uniq",".")

plot_evalue_differences_byfam<-function(blastfile, directory){

  # load blast file---------------------------------------------------------####
  #blastfile<-"pscplos17.blast.uniq"
  blast_genomesvsfam<-read.table(blastfile)

  # shorten the dataframe
  matriz_resultante <- as.data.frame(blast_genomesvsfam[, c(1,2, 11)])
  matriz_resultante$V11<- as.numeric(matriz_resultante$V11)
  matriz_resultante <- matriz_resultante[order(matriz_resultante$V11),]

  # make a list of all families in the file
  Familieslist<- unique(unlist(regmatches(matriz_resultante$V1, gregexpr("\\|(\\d+)\\|", matriz_resultante$V1))))

  # Función para extraer el número
  extraer_numero <- function(nombre) {
    numero <- unlist(regmatches(nombre, gregexpr("\\|(\\d+)\\|", nombre)))
    return(numero)
  }

  # Reemplazar la columna 'nombres' con los números
  matriz_resultante <- matriz_resultante %>%
    mutate(numeros = extraer_numero(V1))

  ## calculate evalues to NP------------------------------------------------####
  archivosGenomesvsNP <- as.list(list.files(directory,
                                            pattern = "^.*ExpandedVsNp\\.blast\\.2$"))
  archivosGenomesvsNP  <-as.list(paste0(directory, archivosGenomesvsNP))
  #print(archivosGenomesvsNP)
  genomeNPvalues<-lapply(archivosGenomesvsNP,
                         make_fam_np_evalue,
                         blast_genomesvsfam)

  # Usa bind_rows para combinar los data frames
  mm_NP_df <- bind_rows(genomeNPvalues)
  #print(mm_NP_df)
  # combined_data ahora contiene todos los datos de los data frames en meanmedian_data

  #mm_NP_df <- do.call(rbind, genomeNPvalues)
  #print(mm_NP_df)

  # calculate mean and median of the diferences between e-values------------####
  meanmedian_data<-lapply(Familieslist,
                          make_evalue_df_forplot_byfamily,
                          matriz_resultante,
                          mm_NP_df )

  result_mm_df <- bind_rows(meanmedian_data)
  #print(result_mm_df)
  #data_filtrado <- result_mm_df %>%
  #  filter(abs(CENTRALmedian - closest5NPmedian) <= 0) %>% filter(CENTRALmedian >= 0)
  #print(data_filtrado)

  # format the data to see in the same plot---------------------------------####
  data_long_mm <- gather(result_mm_df,
                         key = "columna",
                         value = "valor",
                         -Familia)
  df_filtrado <- data_long_mm[complete.cases(data_long_mm), ]

  # make a plot to compare means and medians
  #mmplotfam <- ggplot(data_long_mm, aes(x = Familia, y = valor, fill = columna)) +
  #  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  #  labs(title = "Comparación de diferencias de e-valores en -log10 por familia",
  #       x = "Nombres",
  #       y = "Valor",
  #       fill = "Columna") +
  #  #scale_y_log10(oob = scales::squish_infinite) +  # Ajuste de escala en eje y
  #  theme_minimal() +  # Rotar etiquetas en eje x
  #  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  # Definir los colores manualmente
  mis_colores <- c("#0e95d0","#0e95d0","#0e95d0","#0e95d0","#0e95d0","#0e95d0","#0e95d0","#FFC0CB","#DC143C","#4169E1","#1E90FF","#6495ED")

  #paleta_azul <- c("#03396c", "#005b96", "#0a7bc2", "#0e95d0", "#13afd7", "#18c2e0", "#1ce2f0")

  #paleta_azul_claro <- c("#C6DBEF", "#9EC8E9", "#7BAFD4", "#5B94C1", "#4377B0", "#2B5FA3", "#144592")

  mmplotfam<-ggplot(df_filtrado, aes(x = Familia, y = valor, fill = columna)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    labs(title = "Comparison of differences in e-values by family",
         x = "Enzyme Families",
         y = "e-value -log10",
         fill = "Column") +
    scale_fill_manual(values = mis_colores) +  # Asignar los colores personalizados
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(mmplotfam)
}
