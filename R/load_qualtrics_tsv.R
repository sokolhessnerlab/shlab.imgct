#' Load Qualtrics Data Export (TSV)
#'
#' \code{load_qualtrics_tsv} Load an exported dataset from Qualtrics that is in
#' TSV format.
#'
#' @param path The path string to the qualtrics export to load.
#' @param filename The string name of the TSV file, including the extension,
#' that the exported data was saved with at the  directory of the path
#' provided. Defaults to "qualtrics_export.tsv".
#'
#' @return qualtrics_df A dataframe of the raw exported Qualtrics data without
#' any processing.
#'
#' @examples
#' load_qualtrics_tsv("/path/to/qualtrics_export")
#'
#' @export
load_qualtrics_tsv <- function(path, filename = "qualtrics.tsv") {
  qualtrics_df <- read.delim(
    file.path(
      path,
      "raw",
      filename
    ),
    header = TRUE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    na = "NA",
    fileEncoding = "UTF-16LE"
  )
  return(qualtrics_df)
}


