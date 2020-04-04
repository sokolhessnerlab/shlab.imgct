#' Load block from file
#' 
#' \code{load_block} reads a block into a dataframe.
#'
#' @param path String for relative path.
#' @param block_template String literal for block template.
#' 
#' @return
#'
#' @examples
#' load_block("./data/raw/raw_01.tsv", block_template = "RAW")
#' load_block("./data/clean/clean_07.tsv", block_template = "CLEAN")
#' 
#' @export
load_block <- function(path, block_template = c("RAW", "CLEAN", "VALID")) {

	block_template <- match.arg(block_template)

	if (block_template == "RAW") return(load_raw_block(path))
	else if (block_template == "CLEAN") return(load_clean_block(path))
	else if (block_template == "VALID") return(load_valid_block(path))

}

#' \code{load_raw_block}
#' @return raw_block
load_raw_block <- function(path) {
  raw_block <- read.delim(
    path,
    header = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-16LE"
  )
  return(raw_block)
}

#' \code{load_clean_block}
#' @return clean_block
load_clean_block <- function(path) {
	block <- readr::read_tsv(
    path_to_clean_block, 
    col_names = TRUE, 
    col_types = readr::cols(.default = readr::col_character())
  )
}

#' \code{load_valid_block}
#' @return valid_block
load_valid_block <- function(path) {
	# mirrors functionality?
	block <- load_clean_block(path)
}