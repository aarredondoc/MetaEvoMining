files_list <- list.files(path)
path<-"Alcanivoracaceae/alg_intersection"
getwd()
file.path(getwd(),path, files_list)
results_list <- lapply(files_list, function(file) {
  file_path <- file.path(getwd(),path, file)
  if (file.info(file_path)$is.file) {
    x <- stri_read_lines(file_path)
    list(file = file, matches = stri_detect_regex(x, paste(search_strings, collapse = "|")))
  } else {
    list(file = file, matches = NULL)
  }
})

# view the list of results
results_list <- list.files()
results_list <- lapply(files_list, function(file) {
  file_path <- file.path(getwd(), file)
  if (file.info(file_path)$is.file) {
    x <- stri_read_lines(file_path)
    list(file = file, matches = stri_detect_regex(x, paste(search_strings, collapse = "|")))
  } else {
    list(file = file, matches = NULL)
  }
})

# view the list of results
results_list
