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
  #resultsdataframe <- data.frame(
   # Familia = unique(subconjunto$numero),
  #  CENTRALmean= -log10(mean_dif+1e-200),
   # CENTRALmedian =-log10(median_dif+1e-200)
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
  #)
  # Crear un DataFrame llamado resultsdataframe
  # Crear un DataFrame llamado resultsdataframe
  # Crear un DataFrame llamado resultsdataframe
  # Crear un DataFrame llamado resultsdataframe con Familia como clave
  resultsdataframe <- data.frame(
    Familia = unique(subconjunto$numero),
    CENTRALmean = mean_dif,
    CENTRALmedian = median_dif
  )

  # Combinar los DataFrames por la columna "Familia" y agregar las columnas calculadas de mm_NP_df
  resultsdataframe <- merge(resultsdataframe, mm_NP_df, by="Familia", all.x=TRUE)


  transformacion <- function(x) {
    if (!is.na(x) && !identical(names(x), "Familia")) {
      return(-log10(x + 1e-200))
    } else {
      return(x)
    }
  }

  # Aplicar la transformación a todas las columnas excepto "Familia"
  for (colname in colnames(resultsdataframe)) {
    if (colname != "Familia") {
      if (colname == "NPmean") {
        resultsdataframe[[colname]] <- -log10(abs(resultsdataframe[[colname]] - resultsdataframe[["CENTRALmean"]]) + 1e-200)
      } else if (!identical(colname, "CENTRALmedian") && !identical(colname, "CENTRALmean")) {
        resultsdataframe[[colname]] <- transformacion(resultsdataframe[[colname]])
      }
    }
  }

  for (colname in colnames(resultsdataframe)){
    if (colname == "CENTRALmedian" || colname == "CENTRALmean") {
      resultsdataframe[[colname]] <- transformacion(resultsdataframe[[colname]])
    }
  }
  # Definir la función de transformación

  # Aplicar la función de transformación selectivamente a las columnas de resultsdataframe
  #for (colname in colnames(resultsdataframe)) {
   # if (colname != "Familia") {
      #if (colname == "mean_allNP") {
       # resultsdataframe[[colname]] <- transformacion(abs(resultsdataframe[[colname]] - resultsdataframe[["CENTRALmean"]]))
      #} else if (colname == "CENTRALmedian" || colname == "CENTRALmean") {
       # resultsdataframe[[colname]] <- transformacion(resultsdataframe[[colname]])
      #} else {
     #   resultsdataframe[[colname]] <- -log10(abs(resultsdataframe[[colname]] - resultsdataframe[["CENTRALmedian"]]) + 1e-200)
    #  }
   # }
  #}

  #resultsdataframe[, -1] <- apply(resultsdataframe[, -1], 2, transformacion)
  print(resultsdataframe)

  #for (colname in colnames(resultsdataframe)) {
  #if (colname != "Familia") {
    #if (colname == "mean_allNP") {
   #   resultsdataframe[[colname]] <- -log10(abs(resultsdataframe[[colname]] - resultsdataframe[["CENTRALmean"]]) + 1e-200)
  #  } else if (!identical(colname, "CENTRALmedian") && !identical(colname, "CENTRALmean")) {
     # resultsdataframe[[colname]] <- -log10(abs(resultsdataframe[[colname]] - resultsdataframe[["CENTRALmedian"]]) + 1e-200)
    #} else {
   #   resultsdataframe[[colname]] <- transformacion(resultsdataframe[[colname]])
  #  }
 # }
#}
  #print(resultsdataframe)
  # Ahora, calcula las diferencias y aplica la transformación -log10 en las columnas específicas

  #print(mm_NP_df)
  # Incluir todas las columnas calculadas en resultsdataframe
  #resultsdataframe <- cbind(resultsdataframe, mm_NP_df)


  # Incluir todas las columnas calculadas en resultsdataframe
  #resultsdataframe <- cbind(resultsdataframe, mm_NP_df)




  #print(resultsdataframe)
  #calculate differences NP
  #mm_NP_df$closest5NPmedian <- -log10(abs(median_dif - mm_NP_df$closest5NPmedian))
  #mm_NP_df$closestmean5NP <- -log10(abs(mean_dif - mm_NP_df$closestmean5NP))
  #mm_NP_df$closestNP <- -log10(abs(median_dif - mm_NP_df$closestNP))
  #print(mm_NP_df)

  # Reemplazar la columna 'nombres' con los números
  #mm_NP_df <- mm_NP_df %>%

  #  mutate(numeros = extraer_numero(Familia))

  # add cols by index
  #match_indices <- match(resultsdataframe$Familia,mm_NP_df$Familia)
  #print(match_indices)
  # Agregar la columna Valor2 de df2 a df1 basado en las correspondencias
  #resultsdataframe$closestmean5NP <- mm_NP_df$closestmean5NP[match_indices]
  #resultsdataframe$closest5NPmedian <- mm_NP_df$closest5NPmedian[match_indices]
  #resultsdataframe$closestNP <- mm_NP_df$closestNP[match_indices]
  # Omit NA
  #resultsdataframe<-na.omit(resultsdataframe)
  #print(resultsdataframe)


  return(resultsdataframe)
}
#closest5NPmedian = median_dif - mm_NP_df$closest5NPmedian
