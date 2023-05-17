#' @title Group files by taxa
#' @description This function takes a list of rast-alike ids and copies all
#' those files matching list to a destination directory
#' @usage make_directories_by_taxa(inputdir,taxa)
#' @param inputdir is the  path of the input files directory
#' @param taxa is the taxonomic family
#' @details This function uses a _bins.IDs file for specie and list the files
#' that match with those genomes.the function also makes a parent directory with
#' the name of the family, and another dir inside of them, and copies all the
#' files selected in the list. After that, copy the bins.IDs file to family
#' directory and finally remove all files that has already copied.
#' @import readr dplyr
#' @examples
#' \dontrun{
#' make_directories_by_taxa("data/","f__Saccharospirillaceae")
#' }
#' @noRd

make_directories_by_taxa<-function(inputdir,taxa){
#make the name of family directory and create it----------------------------####
file_name_var <- taxa
fam_nm<-strsplit(file_name_var,"__")
fam_name <-fam_nm[[1]][2]

parent_directory<-dir.create(paste0("data/",fam_name))
#make the name of output directory------------------------------------------####
get_dir_name <- function() {
  #today <- format(Sys.Date(), "%Y-%m-%d")
  return(paste0("data/",fam_name,"/",fam_name,"_bins"))
}

# Create the directory using the function to generate the name--------------####
dir_name <- get_dir_name()
targetdir <- dir.create(dir_name)
targetdir
dir_name

# load the bins.IDs file----------------------------------------------------####
fileIDS <- list.files("data/",pattern = c(paste(fam_name,"_bins.IDs",sep="")))
df <- readLines(c(paste("data/",fileIDS,sep="")))

# search for the first element which is the name and makes a list------------####
v1<-strsplit(df, '\t')
listofnames<-lapply(v1,`[[`, 1)
listofnames

# 1. list all the filesin the input directory-------------------------------####
filestocopy <- gsub(".txt","",
                    gsub(".faa","",
                         list.files(inputdir,full.names = TRUE)))

# 2. keep only the files that match with the .faa and .txt patterns---------####
filestocopy <- unique(grep(paste(listofnames,collapse="|"),
                           filestocopy,
                           value=TRUE))

# 2.1 put the list of .faa and .txt patterns together
faa <- paste(filestocopy,".faa",sep = "")
txt <- paste(filestocopy,".txt",sep = "")
filestocopy<-c(faa,txt)



# 3. copy the listed files--------------------------------------------------####
sapply(filestocopy, function(x) file.copy(from=x, to=dir_name, copy.mode = TRUE))


# 4. copy the bins.IDs file to parent directory-----------------------------####
binsidfile<-c(paste("data/",fileIDS,sep=""))

file.copy(from=binsidfile,
          to=paste0("data/",fam_name,"/"),
          copy.mode = TRUE)

# 5. remove all files that already copied--------------------------------------####
files_to_remove<-c(filestocopy,binsidfile)

unlink(files_to_remove,
       recursive = FALSE,
       force = FALSE)

}
