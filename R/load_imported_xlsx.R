#' Load Imported XLSX
#'
#' \code{load_imported_xlsx} loads the .xlsx file at specified full path to the
#' the "imports" subdirectory of the desired data path.
#'
#' @param path The path string to the parent directory in which the all data
#' files exists. 
#' @param import_filename The filename of the result desired. Defaults to
#' XLSX, and no XLSX extension is required, but is accepted. Other file
#' extensions are not accepted.
#' @param columns Types of columns desired when loading imported dataset.
#' Defaults to guesses by readxl interpreter. Override default guesses with 
#' a vector of column types, such as c("guess", "text", "guess", "skip").
#'
#' @return result A dataframe consisting of the imported database file's contents.
#'
#' @examples
#' load_imported_xlsx(path, "iaps_emotion_ratings")
#'
#' @export
load_imported_xlsx <- function(path,
                               import_filename,
                               columns = NULL) {


  if (stringr::str_ends(import_filename, ".xlsx", negate = TRUE)) {
    import_filename <- stringr::str_c(import_filename, ".xlsx")
  }

  filename <- file.path(path, "imports", import_filename)

  if (!file.exists(filename)) {
    return("The file you are attempting to load doees not exist,
           or the file extension you've chosen is not an XLSX.")
  }

  import <- readxl::read_xlsx(
    filename,
    col_names = TRUE,
    trim_ws = TRUE,
    col_types = columns
  )

  return(import)
}
