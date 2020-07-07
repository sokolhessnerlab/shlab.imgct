#' Calculate heterogeneity index for category responses
#'
#' \code{calculate_htg_index} calculates a measure of heterogeneity
#' for a given image's category response counts in a single row.
#' 
#' @param row A list of counts by category for a given image.
#'
#' @return A measure of variance relative to the heterogeneity of repsonses.
#'
#' @export
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
