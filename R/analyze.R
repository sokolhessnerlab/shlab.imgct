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
analyze <- function(path, filename = "categorized_0_valid.tsv") {
  
  # this goes to a singular TSV file, as opposed to a directory
  path_to_categorized <- file.path(path, filename)
  df <- readr::read_tsv(path_to_categorized)

  df <- df %>%
    dplyr::mutate(
      htg_index = select(., -c(image_id)) %>%
        purrr::pmap_dbl(~shlab.imgct::calculate_htg_index(c(...)))
      ) %>%
      dplyr::arrange(desc(htg_index))

  return(df)

}
