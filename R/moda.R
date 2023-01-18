#cargamos la tabla
library("readr")
evominining_table <- read_tsv('Evominingtable.tsv')
Totalgenomes<- evominining_table$Genomes
Totalgenomes[]
class(Totalgenomes)
genomas <- c("Pseudophaeobacter_5mSIPHEX1_37", "Paracoccus_sp000967825_700mSIPHEX1_1")
Totalgenomes


columnGLU6P
class(columnGLU6P)

getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda <- getmoda(columnGLU6P)

getmode(columnGLU6P)
print(moda)
columnGLU6P==moda


which(columnGLU6P==moda)



row.names
==genomas

