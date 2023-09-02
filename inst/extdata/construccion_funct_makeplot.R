blast_genomesvsfam<-read.table("pscplos17.blast.uniq")



directory<- "."
archivosGenomesvsNP <- as.list(list.files(directory, pattern = "*ExpandedVsNp.blast.2"))
class(archivosGenomesvsNP)

genomeNPvalues<-lapply(archivosGenomesvsNP, make_fam_np_evalue, blast_genomesvsfam)
mm_NP_df <- do.call(rbind, genomeNPvalues)




blast_genomesvsfam<-read.table("pscplos17.blast.uniq")
blast_genomesvsnp<-read.table("1.fasta_ExpandedVsNp.blast.2")



# Encontrar las correspondencias de IDs entre df1 y df2
matching_indices <- match(blast_genomesvsnp$V1,blast_genomesvsfam$V2)

# Agregar la columna Valor2 de df2 a df1 basado en las correspondencias
blast_genomesvsnp$Familia <- blast_genomesvsfam$V1[matching_indices]

blast_genomesvsnp<-na.omit(blast_genomesvsnp)

blast_genomesvsnp <- blast_genomesvsnp[order(blast_genomesvsnp$V11),]
first5_np<-slice_head(n = 5, blast_genomesvsnp)
first5_np<- first5_np[,c(2,11,13)]

closestNP<-first5_np[1,]

mean_difNP<-mean(first5_np$V11)
median_difNP<-median(first5_np$V11)

NP_evalue_dataframe <- data.frame(
  Familia = unique(first5_np$Familia),
  media = mean_dif,
  mediana = median_difNP,
  closestNP = closestNP[,"V11"]
)


library(ggplot2)
library(tidyr)
library(scales)
# master function

blast_genomesvsfam<-read.table("pscplos17.blast.uniq")


plotmm1<- plot_evalue_differences("pscplos17.blast.uniq",".")






#blast_genomesvsnp<-read.table("1.fasta_ExpandedVsNp.blast.2")


matriz_resultante <- as.data.frame(blast_genomesvsfam[, c(1,2, 11)])
matriz_resultante$V11<- as.numeric(matriz_resultante$V11)
matriz_resultante <- matriz_resultante[order(matriz_resultante$V11),]
#first5_np<-slice_head(n = 5, matriz_resultante)
Familieslist<-unique(matriz_resultante$V1)



#class(matriz_resultante$V11)
#transform_value <- function(value) {
#  -log10(value)  # Invertir y tomar el logaritmo en base 10
#}

# Filtrar filas basadas en el patrón en la columna SeqID

#grepl("*D3phosphoglycerate_dehydrogenase*", matriz_resultante$V1)

#grepl("*D3phosphoglycerate_dehydrogenase*", matriz_resultante$V1)
unicos<-unique(matriz_resultante$V1)
#unicos[grepl("*Vancomycin_resistance_protein_VanH_*", unicos)]
subconjunto <- matriz_resultante[matriz_resultante$V1 == "3PGA_AMINOACIDS|2|Phosphoserine_aminotransferase_2|none", ]
subconjunto <- subconjunto[order(subconjunto$V11),]
library(dplyr)
first5_np<-slice_head(n = 5, subconjunto)


subconjunto1 <- matriz_resultante[grepl("*D3phosphoglycerate_dehydrogenase*", matriz_resultante$V1), ]
subconjunto1 <- subconjunto1[order(subconjunto1$V11),]
unique(subconjunto$V1)
first5_np<-slice_head(n = 5, subconjunto1)

#subconjunto$TransformedValor<-transform_value(subconjunto$V11)
diferencias <- abs(diff(subconjunto$V11))

#epsilon <- 1e-150
mean_dif<-mean(first5_np$V11)
median_dif<-median(first5_np$V11)




resultsdataframe <- data.frame(
  NP = unique(first5_np$V2),
  media = -log10(mean_dif) + 1e-200,
  mediana = -log10(median_dif)+ 1e-200

)

library(tidyr)
data_long_r <- gather(resultsdataframe, key = "columna", value = "valor", -Familia)

library(ggplot2)
ggplot(data_long_, aes(x = Familia, y = valor, fill = columna)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Comparación de Valores en Notación Científica",
       x = "Nombres",
       y = "Valor",
       fill = "Columna") +
  #scale_y_log10() +  # Ajuste de escala en eje y
  theme_minimal() +  # Rotar etiquetas en eje x
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


meanmedian_data<-lapply(Familieslist,
                        make_evalue_df_forplot,
                        matriz_resultante,
                        mm_NP_df )

result_mm_df <- do.call(rbind, meanmedian_data)
