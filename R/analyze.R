#' Analyze Function
#'
#' \code{analyze} loads counted categories and determines the best category for
#' each image.
#'
#' @param path The path relative to working directory that holds data in
#' designated "categorized" directories.
#' 
#' @return A data frame with two columns composed of (image_id, category)
#' tuples.
#'
#' @examples
#'
#' @export
analyze <- function(path) {
  
  # this goes to a singular TSV file, as opposed to a directory
  path_to_categorized <- file.path(path, "categorized", "categorized.tsv")
  df <- readr::read_tsv(path_to_categorized)
  return(df)

}