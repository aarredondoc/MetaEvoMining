#' @Title Make a dataframe with blast e-values genomesVs.NP
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

  # Obtener información sobre el archivo
  info_archivo <- file.info(blast_genomesvsNP)
  #print(info_archivo)
  # Verificar si el tamaño del archivo es cero
  if (info_archivo$size == 0) {
    #cat("El archivo está vacío o tiene 0 filas. Pasando al siguiente archivo.\n")

  } else {
    #blast_genomesvsfam<-read.table(blast_genomesvsFam)
    blast_genomesvsnp<-read.table(blast_genomesvsNP)

    # Encontrar las correspondencias de IDs entre df1 y df2-------------------####
    matching_indices <- match(blast_genomesvsnp$V1, blast_genomesvsfam$V2)
    #print(matching_indices)

    # Agregar la columna Valor2 de df2 a df1 basado en las correspondencias
    blast_genomesvsnp$Familia <- blast_genomesvsfam$V1[matching_indices]
    #print(blast_genomesvsnp)

    # Omit NA
    blast_genomesvsnp<-na.omit(blast_genomesvsnp)
    #print(blast_genomesvsnp)
    if (nrow(blast_genomesvsnp) == 0) {

      #cat("El archivo está vacío o tiene 0 filas. Pasando al siguiente archivo.\n")


    } else {
      # Order and slice to 5 and best e values----------------------------------####
      blast_genomesvsnp <- blast_genomesvsnp[order(blast_genomesvsnp$V11),]

      blast_genomesvsnp <- blast_genomesvsnp %>%
        mutate(numeros = extraer_numero(Familia))

      lista_de_dataframes <- split(blast_genomesvsnp,blast_genomesvsnp$V2)
      #print(lista_de_dataframes)
      medianas <- sapply(lista_de_dataframes, function(df) median(df$V11))
      #medias<-order[medias]
      medianas_ordenadas <- sort(medianas)
      # Imprime los nombres de los dataframes
      nombres_dataframes_top5 <- names(medianas)[match(head(medianas_ordenadas, 5), medianas)]
      #print(nombres_dataframes_top5)
      #medianastop5<-medianas[nombres_dataframes_top5]
      #print(medianastop5)
      #medianas_seleccionadas <- numeric(length(nombres_dataframes_top5))
      # Crear un nuevo dataframe vacío con los nombres de las medianas como columnas
      # Suponiendo que tienes un vector de nombres llamado nombres_dataframes_top5
      # y un vector de medianas llamado medianas
      # Suponiendo que tienes un vector de nombres llamado nombres_dataframes_top5
      # y un vector de medianas llamado medianas

      # Crear un nuevo dataframe
      nuevo_dataframe <- data.frame(Familia = unique(blast_genomesvsnp$numeros))

      # Agregar las medianas como columnas al nuevo dataframe
      for (i in 1:length(nombres_dataframes_top5)) {
        nombre <- nombres_dataframes_top5[i]
        indice <- match(nombre, names(medianas))
        nueva_columna <- medianas[indice]
        nombre_columna <- nombres_dataframes_top5
        nuevo_dataframe[nombre_columna] <- nueva_columna
      }

      # Ahora, nuevo_dataframe contendrá una columna "Nombre" con los nombres de los dataframes
      # y una columna separada para cada mediana

      print(nuevo_dataframe)
      # El nuevo_dataframe tendrá una columna por cada mediana
      # con los nombres de los dataframes como encabezados de columna


      # Inicializa objetos para almacenar las medianas seleccionadas
      #medianas_seleccionadas <- numeric(length(nombres_dataframes_top5))

      # Filtra y guarda las medianas correspondientes en objetos separados
      #for (i in 1:length(nombres_dataframes_top5)) {
       # nombre <- nombres_dataframes_top5[i]
        #indice <- match(nombre, names(medianas))
        #medianas_seleccionadas[i] <- medianas[indice]
      #}
      #print(medianas_seleccionadas)
      # Ahora, medianas_seleccionadas contendrá las medianas de los 5 dataframes seleccionados,
      # cada una en un elemento separado del vector

      #filter_medianas
      #dataframes_filtrados <- lista_de_dataframes[nombres_dataframes_top5]

      #print(dataframes_filtrados)


      #crop to the top5
      first5_np<-slice_head(n = 5, blast_genomesvsnp)

      #lista_de_dataframes <- split(first5_np,first5_np$V2)
      #print(lista_de_dataframes)

      first5_np<- first5_np[,c(2,11,13)]
      closestNP<-first5_np[1,]
      #print(unique(blast_genomesvsnp$V2))


      # Calculate mean and median of top 5
      mean_NP<-mean(first5_np$V11)
      #print(mean_NP)
      #mean_NP<-
      median_NP<-median(first5_np$V11)

      # make a result datframe--------------------------------------------------####
      NP_evalue_dataframe <- data.frame(
        Familia = unique(blast_genomesvsnp$numeros),
        closestmean5NP = mean_NP + 1e-200,
        closest5NPmedian = median_NP + 1e-200,
        closestNP = closestNP[,"V11"] + 1e-200
      )

      print(NP_evalue_dataframe)
      # Suponiendo que A y B son tus dataframes
      combined_df <- merge(NP_evalue_dataframe, nuevo_dataframe, by = "Familia", all.x = TRUE)
      print(combined_df)
    }
  }

}





