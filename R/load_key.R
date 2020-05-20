#' Load Key from TXT file
#'
#' \code{load_key} reads a list of key=value pairs in a txt file where "=" is
#' the required delimeter.
#'
#' @param path String path to the project root directory.
#' @param filename Name of the file with extension .txt.
#' @param col_names Vector of the key and value headers. Defaults to "key"
#' and "value" via c("key", "value").
#' 
#' @return A transposed key.
#'
#' @examples
#' load_key("/path/to/keys", filename = "validation_key.txt")
#' load_key("/path/to/keys", filename = "categorization_key.txt")
#'
#' @export
load_key <- function(path, filename, col_names = c("key", "value")) {
  
  txt <- readr::read_delim(
    file.path(path, "keys", filename),
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
