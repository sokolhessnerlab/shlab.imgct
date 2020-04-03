#' Load a key from txt file
#'
#' \code{load_txt_key} reads a list of key=value pairs in a txt file.
#'
#' @param path must be a relative path to the project root directory 
#' @param key_type can equal "category" or "validation"
#'
#' @return 
#'
#' @examples
#'
#' @export
load_txt_key <- function(path, 
                         key_template = c("CATEGORY", "VALIDATION"), 
                         col_names = c("key", "value")) {
  
  library("readr")
  library("tidyr")

  key_template <- match.arg(key_template)

  key_path <- file.path(path)

  txt <- readr::read_delim(
    key_path, 
    "=", 
    col_names = col_names,
    col_types = readr::cols(.default = col_character()),
  )
  
  key <- do.call(
    tidyr::spread, 
    list(txt, col_names[1], col_names[2])
  )

  return(key)

}