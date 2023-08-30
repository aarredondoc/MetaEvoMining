#' @title Group files by taxa
#' @description This function moves the files created with the make_directories
#' @usage move_directories(dir_paths, target_dir_path)
#' @param dir_paths is a list of directories to move
#' @param target_dir_path is a target directory
#' @details This function uses the file.rename to move directories in the
#' make_Evo_Files funtion.
#' @examples move_directories(directories_toMove, "data/Results")
#' @noRd

move_directories <- function(dir_paths, target_dir_path) {
  # create the target directory if it doesn't already exist-----------------####
  if (!file.exists(target_dir_path)) {
    dir.create(target_dir_path)
  }

  # move each directory to the target directory-----------------------------####
  for (dir_path in dir_paths) {
    dir_name <- basename(dir_path)
    new_dir_path <- file.path(target_dir_path, dir_name)
    file.rename(from = dir_path, to = new_dir_path)
  }
}
