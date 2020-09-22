#' Make Image List Function
#'
#' \code{make_image_list} Creates and writes a TXT list of image names.
#'
#' @param path The path relative to working directory where database
#' images are stored in multiple directories, each named for the database. 
#' @param list_name String name of list for output file. Defaults to
#' "image_list".
#' 
#' @return List of image file names with prefix database identifiers.
#'
#' @examples
#' make_image_list_from_dirs("imgct/images/databases")
#'
#' @export
make_image_list_from_dirs <- function(path, list_name = "img_list") {
  img_list <- c()
  for (dir in list.files(path)) {
    subpath <- file.path(path, dir)
    for (f in list.files(subpath)) {
      f <- tolower(f)
      f <- stringr::str_replace_all(f, " ", "_")
      fn <- paste0(toupper(dir), "_", f)
      img_list <- c(img_list, fn)
    }
  }
  write.table(img_list, paste0(list_name, ".txt"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE)
  return(img_list)
}
