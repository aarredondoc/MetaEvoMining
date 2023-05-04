fasta_lines <- readLines("new2.fasta")

# filter out the blank lines
non_blank_lines <- fasta_lines[!grepl("^\\s*$", fasta_lines)]

# write the non-blank lines back to the file
writeLines(non_blank_lines, "noblank_DB.fasta")
