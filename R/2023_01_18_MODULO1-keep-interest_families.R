
library("readr")

#encontrar la moda de una columna-------------------------------------------####
getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##above_mode_fun------------------------------------------------------------####

#input a vector that contains a column
#input2 genomas de interes
#output verdadero o falso si los genomas se encuentran en la columna por arriba de la moda
above_mode_fun <- function(f,g_names){#f is a vector and df is a dataframe
  EFmode <- getmoda(f)
  abovemodeindexes <- which(f > EFmode)
  binsabovemode<-g_names[which(g_names%in%abovemodeindexes)]
  trueorfalse<-any(binsabovemode)
  return(trueorfalse)

}

#above_number_mode_fun------------------------------------------------------####

#input a vector that contains a column
#input2 genomas de interes
#output verdadero o falso si los genomas se encuentran en la columna por arriba de la moda
#y que la moda es mayor a 0
above_number_mode_fun <- function(f,g_names){#f is a vector and df is a dataframe
  EFmode <- getmoda(f)
  if(EFmode > 0){
    abovemodeindexes <- which(f > EFmode)
    binsabovemode<-g_names[which(g_names%in%abovemodeindexes)]
    trueorfalse<-any(binsabovemode)
    } else {
      trueorfalse<-FALSE
    }
  return(trueorfalse)
}

#variables------------------------------------------------------------------####
evominining_table <- read_tsv('Flavobacteraceae_evomining_table.tsv')#dataframe
EF <- evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`#example family column 1
EF2<-evominining_table$`Glycolysis_6--glyceraldehyde3phosphate_dehydrogenase_4`#example family column 2
EF3<-evominining_table$'Glycolysis_1--Glucose_kinase_7' #example family column 3
Totalgenomes<- evominining_table$Genomes #all genome names list
Mydata_bins <- grep("*mSIPHEX*",Totalgenomes) # My genomes of interest

#main program---------------------------------------------------------------####
above_mode_fun(EF2,Mydata_bins)
above_number_mode_fun(EF3, Mydata_bins)
#ejemplos--------------------------------------------------------------------####
evominining_table$Genomes[336]#"Tateyamaria_sp900143535_5mSIPHEX1_8"
#1
evominining_table$`ALPHAKETOGLUTARATE_AMINOACIDS_3--Glutamine_synthetase_1`[336]#6
getmoda(EF)#4
#2
evominining_table$`Glycolysis_6--glyceraldehyde3phosphate_dehydrogenase_4`[336] #3
getmoda(EF2) #3
#3
evominining_table$'Glycolysis_1--Glucose_kinase_7'[336] #1
getmoda(EF3) #0

#select all numeric columns-------------------------------------------------####
library("dplyr")
numerical_columns <- sapply(evominining_table, is.numeric)
class(numerical_columns)
numco<-evominining_table[numerical_columns]#selecciona todas las columnas numericas
#function getmoda applied to all columns of a dataframe---------------------####
allmodes<-sapply(numco,getmoda)#lista de nombres de columnas y las modas de todas las columnas

#funtion above_mode_fun applied to all columns of a dataframe----------------------------####

keepindex<-sapply(numco,above_mode_fun,Mydata_bins)#arroja una lista de nombres con TRUE si esta en los genomas de interes y FALSE si no está
keepcolumns<-numco[keepindex] #familias a las que si hacer el arbol de evomining
nameskeepcol<-colnames(keepcolumns)

#create a file with the EF that we want to keep-----------------------------####
write.table(nameskeepcol, "runtreeevo_Flavobacteraceae.txt", sep = "\t", row.names = FALSE, col.names = FALSE,quote = FALSE)

#funtion above_number_mode_fun applied to all columns of a dataframe---------####

indicesaboveone<-sapply(numco,above_number_mode_fun,Mydata_bins)
modestokeep<-allmodes[indicesaboveone]
length(allmodes[indicesaboveone])
names(allmodes)[1]
which(keepindex==TRUE)

length(which(keepindex==TRUE))


