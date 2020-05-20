#' Load Qualtrics Export (TSV)
#'
#' \code{load_qualtrics_export}
#'
#' @param path The path string to the qualtrics export to load.
#' @param filename The string name of the TSV file, including the extension,
#' that the exported data was saved with at the  directory of the path
#' provided. Defaults to "qualtrics_export.tsv".
#'
#' @return qualtrics_export A dataframe of the raw exported Qualtrics data without
#' any processing.
#'
#' @examples
#' load_qualtrics_export("/path/to/qualtrics_export")
#'
#' @export
load_qualtrics_export <- function(path, filename = "qualtrics_export.tsv") {
  qualtrics_export <- read.delim(
    file.path(
      path,
      filename
    ),
    header = TRUE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    na = "NA",
    fileEncoding = "UTF-16LE"
  )
  return(qualtrics_export)
}


