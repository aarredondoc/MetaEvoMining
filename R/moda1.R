library("readr")
#encontrar la moda de una columna
getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmoda(numerical_columns)
#input a vector that contains a column
#input2 genomas de interes
#output verdadero o falso si los genomas se encuentran en la columna por arriba de la moda
above_mode_fun <- function(f,g_names){#f is a vector and df is a dataframe
  EFmode <- getmoda(f)
  abovemodeindexes <- which(f > EFmode)
  binsabovemode<-g_names[which(g_names%in%abovemodeindexes)]
  #binsabovemode<-g_names%in%abovemodeindexes
  return(binsabovemode)

}
#variables
evominining_table <- read_tsv('Evominingtable.tsv')
EF <- evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`
Totalgenomes<- evominining_table$Genomes
Mydata_bins <- grep("*mSIPHEX*",Totalgenomes)
#main program
above_mode_fun(EF,Mydata_bins)
any(above_mode_fun(EF,Mydata_bins))
#ejemplo
evominining_table$Genomes[336]#"Tateyamaria_sp900143535_5mSIPHEX1_8"
evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`[336]#6
getmoda(EF)#4

#select all columns
library("dplyr")
numerical_columns <- sapply(evominining_table, is.numeric)
class(numerical_columns)
numco<-evominining_table[numerical_columns]
#EF_names<-as.list(select_if(evominining_table, is.numeric))
sapply(numco,getmoda)
sapply(numco,above_mode_fun(x,Mydata_bins))
