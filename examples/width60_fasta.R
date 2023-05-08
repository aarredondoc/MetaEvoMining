library(Biostrings)

# Read in the uniline FASTA file as a DNAStringSet object
my_seqset <- readDNAStringSet("165000.faa")

writeXStringSet(my_seqset, "165000.faa", format = "fasta", width =60)

orig_info <- file.info("187000.faa")

# create a new file with the same format as the original
file.create("new_file.txt")
file.info("new_file.txt") <- orig_info
