#' Clean Function
#'
#' \code{clean} scrubs a list of raw response data consisting of participant
#' codes and images with category choices, where each list item is a TSV file
#' within a directory named "raw". The cleaned data is written to a new "clean"
#' directory with TSV files.
#'
#' @param path The path relative to working directory that holds data in
#' designated "raw" directory, as well as the "clean" directory for written
#' data files.
#' @param filename The file name prefix for the raw data export from a given
#' source, and must include file extension (TSV). Defaults to "qualtrics_export.tsv".
#' @param source_type The source from which the raw data was exported from.
#' Defaults to "qualtrics".
#'
#' @examples
#' clean("../mounts/imgct/data/csn_imgct")
#'
#' @export
clean <- function(path, 
                  filename = "qualtrics_export.tsv", 
                  source_type = "qualtrics") {
  if (source_type == "qualtrics") {
    shlab.imgct::clean_qualtrics_export(
      path,
      filename,
      qualtrics_tag = "_Q10"
    )
  }
}
