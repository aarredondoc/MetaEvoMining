library("readr")
evominining_table <- read_tsv('Evominingtable.tsv')
Totalgenomes<- evominining_table$Genomes
Totalgenomes[]
class(Totalgenomes)
genomas <- c("Pseudophaeobacter_5mSIPHEX1_37", "Paracoccus_sp000967825_700mSIPHEX1_1")
genomas
Mydata_bins <- grep("*mSIPHEX*",Totalgenomes)
Totalgenomes[Mydata_bins]
#localizar indices de los genomas
indexes<-which(Totalgenomes%in%Mydata_bins)
indexes

numerical_columns <- sapply(evominining_table, is.numeric)
#encontrar la moda de una columna
getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#encontrar todas las modas
allmodes<-sapply(evominining_table[, numerical_columns], getmoda)

allmodes
#encontrar una columna
EF <- evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`
EF
EFmode <- getmoda(EF)
EFmode
#Index mayor a la moda de cierta columna
above_mode_indexes <- which(evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`> EFmode)
above_mode_indexes
#Buscamos index de genomas en la lista de indexes mayor a la moda
binsabovemode<-Mydata_bins[which(Mydata_bins%in%above_mode_indexes)]
Totalgenomes[binsabovemode]
#Encontrar nombres de lasEF
EF_names <- colnames(evominining_table)

above_mode_indexes <- function(f){

}
