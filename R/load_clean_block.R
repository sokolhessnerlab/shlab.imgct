#' Load Clean Image Block Responses
#'
#' \code{load_clean_block} loads a clean block at a specified full path to the
#' filename with extension, replaces NA values with 0, and returns the clean
#' block as a dataframe. Note that the data are loaded as character type across
#' all columns.
#'
#' @param path The path string to the clean block to load.
#'
#' @return clean_block A dataframe of the clean block. The dataframe will have
#' NA values replaced by 0.
#'
#' @examples
#' load_clean_block(path)
#'
#' @export
load_clean_block <- function(path) {
	clean_block <- readr::read_tsv(
      path, 
      col_names = TRUE, 
      col_types = readr::cols(.default = readr::col_character())
    ) %>% 
    dplyr::mutate_if(is.character, ~replace(., is.na(.), 0))
  return(clean_block)
}
