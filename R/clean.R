#' Clean Function
#'
#' \code{clean} scrubs a sequenctial list of Qualtrics data export files
#' detailing participant category responses to images within a directory of
#' image blocks.
#'
#' @param x The variable x is something to be clean.
#'
#' @examples
#'
#' @export
clean <- function(path) {
	library("tidyverse")
	# TODO
	print(path)
}

# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

QUALTRICS_EXPORT_TAG <- "_Q10"
IP_TAG <- "IP"
IP_NAME <- "IP Address"
IP_RENAME <- "ip_address"
IMAGE_BLOCK_TAG <- "imageBlock"

RAW_BLOCKS_DIR_NAME = "raw"
CLEAN_BLOCKS_DIR_NAME = "clean"

# -----------------------------------------------------------------------------
# CLEAN: BY BLOCK, BY ALL RAW BLOCKS
# -----------------------------------------------------------------------------

# function clean_raw_block(raw_block, block_id)
clean_raw_block <- function(raw_block, block_id) {
  clean_block <- remove_qualtrics_artifacts(raw_block, block_id)
  return(clean_block)
}

# function clean_all_raw_blocks(path)
clean_all_raw_blocks <- function(path) {
  
  path_to_raw_blocks = file.path(path, RAW_BLOCKS_DIR_NAME)
  path_to_clean_blocks = file.path(path, CLEAN_BLOCKS_DIR_NAME)
  
  filenames <- list.files(
    path = path_to_raw_blocks, 
    pattern = "*.tsv",
    full.names = FALSE
  )
  
  for (fn in filenames) {
    
    path_to_raw_block <- file.path(path_to_raw_blocks, fn)
    raw_block <- load_raw_block(path_to_raw_block)
    block_id <- stringr::str_extract(fn, "\\d+") # extract just block number (i.e., "03")
    
    clean_block <- clean_raw_block(raw_block, block_id)
    
    write.table(
      clean_block,
      file = file.path(
        path_to_clean_blocks,
        str_replace(fn, RAW_BLOCKS_DIR_NAME, CLEAN_BLOCKS_DIR_NAME)
      ),
      sep = "\t",
      append = FALSE, # remove file if already exists and replace
      col.names = TRUE,
      row.names = FALSE # remove automated indices as row names
    )
    
  }
  
}
