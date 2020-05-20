#' Load Clean Image Block Responses
#'
#' \code{load_clean_block}
#'
#' @param path The path string to the clean block to load.
#'
#' @return clean_block A dataframe of the clean block.
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
  )
  return(clean_block)
}
