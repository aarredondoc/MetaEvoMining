fasta_lines <- readLines("noblank_DB.fasta")

# initialize a counter for the sequence headers
header_counter <- 1

# loop over the lines and modify the sequence headers
for (i in 1:length(fasta_lines)) {
  if (startsWith(fasta_lines[i], ">")) {
    # this line is a sequence header
    # extract the unique identifier from the header
    header_id <- gsub(">[^ ]+ ", "", fasta_lines[i], perl=TRUE)
    # construct the new header with the appended number
    split<-stri_split(header_id, fixed = "|",simplify = TRUE)
    new_header <- paste(split[1],header_counter,split[3],split[4],sep="|")
    # replace the old header with the new one
    fasta_lines[i] <- new_header
    # increment the header counter
    header_counter <- header_counter + 1
  }
}

# write the modified character vector back to a new FASTA file
writeLines(fasta_lines, "sequential_numbers.fasta")
