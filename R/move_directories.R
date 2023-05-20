#' @title Group files by taxa
#' @description This function takes the files created with the make_directories
#' @usage move_directories(dir_paths, target_dir_path)
#' @param dir_paths is a list of directories to move
#' @param target_dir_path is a target directory
#' @details This function is part of the MetaEvoMining package
#' @examples move_directories(directories_toMove, "data/Results")
#' @noRd

move_directories <- function(dir_paths, target_dir_path) {
  # Create the target directory if it doesn't already exist
  if (!file.exists(target_dir_path)) {
    dir.create(target_dir_path)
  }

  # Move each directory to the target directory
  for (dir_path in dir_paths) {
    dir_name <- basename(dir_path)
    new_dir_path <- file.path(target_dir_path, dir_name)
    file.rename(from = dir_path, to = new_dir_path)
  }
}

#move_directories(directories_toMove, "data/Results")
