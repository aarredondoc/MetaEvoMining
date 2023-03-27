library(dplyr)
library(readr)
Pangenome_matrix <- read_csv("pangenome_matrix_t0.csv")

# la mitad mas 1. sumar true = no cero
#filtrar
rownames(Pangenome_matrix)<-Pangenome_matrix$Gene
num_matrix <- Pangenome_matrix[,15:26]
row1<-num_matrix[1,15:26, value =TRUE]


V <- as.logical()
sum(V)>length(V)/2
#para todos los renglones
filter(Pangenome_matrix,sum(V)>length(V)/2 )
