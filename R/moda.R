#cargamos la tabla
library("readr")
evominining_table <- read_tsv('Evominingtable.tsv')
Totalgenomes<- evominining_table$Genomes
Totalgenomes[]
class(Totalgenomes)
genomas <- c("Pseudophaeobacter_5mSIPHEX1_37", "Paracoccus_sp000967825_700mSIPHEX1_1")
#localizar indices de los genomas
indexes<-which(Totalgenomes%in%genomas)
indexes

numerical_columns <- sapply(evominining_table, is.numeric)
#encontrar la moda de una columna
getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#encontrar todas las modas
allmodes<-sapply(evominining_table[, numerical_columns], getmoda)

class(allmodes)
EF <- evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`
EFmode <- getmoda(EF)
above_mode_indexes <- which(evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`> EFmode)

indexes%in%above_mode_indexes




d<-subset(evominining_table, evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1` > EFmode)
d
which(d)


ifelse (x >EFmode, print(x),FALSE)





getmode(columnGLU6P)
print(moda)
columnGLU6P==moda


which(columnGLU6P==moda)



row.names
==genomas

