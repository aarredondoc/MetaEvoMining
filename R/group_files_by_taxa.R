#' @title Group files by taxa
#' @description This function takes a list of rast-alike ids and copies all those files
#' matching list to a destination directory
#' @usage make_rastIDs_by_taxa(gtdbK_report,taxa)
#' @param gtdbK_report is a tsv file wich was create by
#' [gtdbtk](https://github.com/Ecogenomics/GTDBTk.git) program.
#' @param taxa is the taxonomic rank
#' @details This function is part of the MetaEvoMining package
#' @import readr dplyr plyr
#' @examples make_rastIDs_by_taxa('data/gtdbtk.bac120.summary.tsv',
#' "f__Saccharospirillaceae")
#' @export
#taxa <- "f__Saccharospirillaceae"
#inputdir<-"data/"
#IDs_file<-"data/Saccharospirillaceae_bins.IDs"

group_files_by_taxa<-function(inputdir,taxa){
#ruta al directorio de entrada
#inputdir  <- "Archivos_convertidos_prueba"
#ruta al directorio de salida
file_name_var <- taxa
fam_nm<-strsplit(file_name_var,"__")
fam_name <-fam_nm[[1]][2]

parent_directory<-dir.create(paste0("data/",fam_name))
#make the name of output directory
get_dir_name <- function() {
  #today <- format(Sys.Date(), "%Y-%m-%d")
  return(paste0("data/",fam_name,"/",fam_name,"_bins"))
}

# Create the directory using the function to generate the name
dir_name <- get_dir_name()
targetdir <- dir.create(dir_name)
targetdir
dir_name

#aqui se carga el archivo de ids rast-alike
fileIDS <- list.files("data/",pattern = c(paste(fam_name,"_bins.IDs",sep="")))
df <- readLines(c(paste("data/",fileIDS,sep="")))
#df
#dividimos las columnas del archivo por tabulador
v1<-strsplit(df, '\t')
#buscamos el primer elemento de la separacion para cada linea
listofnames<-lapply(v1,`[[`, 1)
listofnames

# 1. listamos todos los archivos en el directorio
# podemos listar archivos por un patron especifico añadiendo un patron en el argumento = ".faa" por ejemplo.
filestocopy <- gsub(".txt","",gsub(".faa","",list.files(inputdir,full.names = TRUE))) #full.names = TRUE)

# 2. Conserva solo los arrchivos que coinciden con el patron que pusiste
filestocopy <- unique(grep(paste(listofnames,collapse="|"), filestocopy, value=TRUE))
#tomamos las listas .faa y .txt y las unimos
faa<-paste(filestocopy,".faa",sep = "")
txt<-paste(filestocopy,".txt",sep = "")
filestocopy<-c(faa,txt)
#list

#filestocopy
#paste(listofnames,collapse="|")
# 3. copia los archivos desados
sapply(filestocopy, function(x) file.copy(from=x, to=dir_name, copy.mode = TRUE))
#sapply(filestocopy, function(file) {
#  file.copy(file.path(inputdir, file), file.path(targetdir, file),copy.mode = TRUE)
#})

# copy the IDs file to parent directory-------------------------------------####
binsidfile<-c(paste("data/",fileIDS,sep=""))
file.copy(from=binsidfile, to=paste0("data/",fam_name), copy.mode = TRUE)

#remove all files that already copied--------------------------------------####
files_to_remove<-c(filestocopy,binsidfile)
unlink(files_to_remove, recursive = FALSE, force = FALSE)

}



