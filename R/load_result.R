#' Load Result
#'
#' \code{load_result} loads the file at specified full path to the
#' the "results" subdirectory of the desired data path.
#'
#' @param path The path string to the parent directory in which the validation
#' file exists. This should be the same parent directory for "clean" block data.
#' @param result_filename The filename of the result desired. Defaults to
#' a TSV, and no TSV extension is required, but is accepted. Other file
#' extensions are not accepted.
#' @param columns Types of columns desired when loading a result dataset.
#' Defaults to all character types. Override default with a readr-style cols
#' definition.
#'
#' @return result A dataframe consisting of the result file's contents.
#'
#' @examples
#' load_result(path)
#'
#' @export
load_result <- function(path, 
                        result_filename, 
                        columns = readr::cols(.default = readr::col_character())) {

  if (stringr::str_ends(result_filename, ".tsv", negate = TRUE)) {
    result_filename <- stringr::str_c(result_filename, ".tsv")
  }

  filename <- file.path(path, "results", result_filename)

  if (!file.exists(filename)) {
    return("The file you are attempting to load does not exist, 
           or the file extension you've chosen is not a TSV.")
  }

  result <- readr::read_tsv(
    filename,
    col_names = TRUE,
    col_types = columns
  )

  return(result)

}


