#' Load All Clean Image Block Responses
#'
#' \code{load_all_clean_block} loads all clean blocks at a specified full path to the
#' directory in which they all exist with extension TSV.
#'
#' @param path The path string to the directory in which clean data lives.
#'
#' @return all_clean_blocks A list of dataframes of the clean blocks. 
#' The dataframes will have NA values replaced by 0.
#'
#' @examples
#' load_all_clean_block("path/to/clean")
#'
#' @export
load_all_clean_blocks <-function(path) {
  filenames <- list.files(
    path = file.path(path, "clean"), 
    pattern = "*.tsv",
    full.names = TRUE
  )
  all_clean_blocks <- filenames %>% 
    purrr::map(shlab.imgct::load_clean_block)
  return(all_clean_blocks)
}
