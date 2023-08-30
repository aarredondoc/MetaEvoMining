library(tidyr)
library(ggplot2)
library(dplyr)

blast_bbh<-read.table("vueltalos17.blast.uniq")

matriz_resultante <- as.data.frame(blast_bbh[, c(1,2, 11)])

matriz_resultante$V2<- as.numeric(matriz_resultante$V11)
#arrange(matriz_resultante$V11)
library(stringr)
matriz_resultante$V1 <- str_sub(matriz_resultante$V1, end = 20)


# Pivotar los datos
matriz_pivot <- pivot_wider(data = matriz_resultante, id_cols = 1, names_from = 2, values_from = 3, values_fill = 0)


#promedio_distancias <- mean(matriz_pivot, na.rm = TRUE)

ggplot(matriz_resultante, aes(x = V2, y = V1,
                              fill = V11)) + geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap de Datos en Notación Científica",
       x = "Columna 1",
       y = "Columna 2") + theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()


