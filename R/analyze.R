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

  df <- df %>%
    dplyr::mutate(
      htg_index = select(., -c(image_id)) %>%
        purrr::pmap_dbl(~calculate_htg_index(c(...)))
      ) %>%
      dplyr::arrange(desc(htg_index))
    

  return(df)

}

#' Calculate heterogeneity index for category responses
#' \code{calculate_htg_index} calculates a measure of heterogeneity
#' for a given image's category response counts in a single row.
#' 
#' @param row A list of counts by category for a given image.
#'
#' @return A measure of variance relative to the heterogeneity of repsonses.
calculate_htg_index <- function(row) {

  # hmg = homogeneity factor
  # htg = heterogeneity factor

  total_responses <- purrr::reduce(row, `+`)

  hmg <- ifelse(length(unique(row)) == 1, 
                0, 
                max(row) / total_responses)

  htg <- ifelse(hmg == 0, 
                1,
                (length(which(row != 0)) - 1) / total_responses)

  htg_index <- 1 - hmg * (1 - htg)

  return(signif(htg_index, digits = 3))

}
