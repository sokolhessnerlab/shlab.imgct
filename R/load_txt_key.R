#' Load a key from txt file
#'
#' \code{load_txt_key} reads a list of key=value pairs in a txt file.
#'
#' @param path String path to file, which must be a relative path to the project root directory 
#' @param key_template String literal that can equal "CATEGORY" or "VALIDATION".
#' @param col_names Vector of column names for txt key. Defaults to c("key", "value").
#' 
#' @return A transposed key.
#'
#' @examples
#' load_txt_key("./keys/validation_key.txt", key_template = "VALIDATION")
#'
#' @export
load_txt_key <- function(path, 
                         key_template = c("CATEGORY", "VALIDATION"), 
                         col_names = c("key", "value")) {
  
  key_template <- match.arg(key_template) # this could be useful later

  key_path <- file.path(path)

  txt <- readr::read_delim(
    key_path, 
    "=", 
    col_names = col_names,
    col_types = readr::cols(.default = readr::col_character()),
  )
  
  key <- do.call(
    tidyr::spread, 
    list(txt, col_names[1], col_names[2])
  )

  return(key)

}