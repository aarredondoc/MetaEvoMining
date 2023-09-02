# los mejores hits base de datos pcsplos

blast_genomesvsfam<-read.table("vueltalos17.blast.uniq")

# los mejores de vuelta

blast_genomesvsfam<-read.table("pscplos17.blast.uniq")

# Paso 1: Importar los datos
blast_results <- read.table("pscplos17.blast.uniq")

# Paso 2: Calcular distancias evolutivas
blast_results$Distance <- -3/4 * log(1 - 4/3 * (blast_results$V3 / 100))

Distance <- -3/4 * log(1 - 4/3 * (81.60 / 100))
# Paso 3: Visualizar los resultados
library(ggplot2)
ggplot(blast_results, aes(x = SeqID, y = Distance)) +
  geom_bar(stat = "identity") +
  labs(x = "Secuencias", y = "Distancia Evolutiva", title = "Distancias Evolutivas")


