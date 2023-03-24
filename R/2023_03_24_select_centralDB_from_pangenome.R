library(dplyr)
library(readr)
Pangenome_matrix <- read_csv("pangenome_matrix_t0.csv")

# la mitad mas 1. sumar true = no cero
#filtrar
rownames(Pangenome_matrix)<-Pangenome_matrix$Gene
Pangenome_matrix[,15:26]

V <- as.logical(Pangenome_matrix[,15:26])
sum(V)>length(V)/2
#para todos los renglones
filter(Pangenome_matrix,sum(V)>length(V)/2 )
