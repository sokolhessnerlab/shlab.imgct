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

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# function load_raw_block(path_to_raw_block)
load_raw_block <- function(path_to_raw_block) {
  raw_block <- read.delim(
    path_to_raw_block,
    header = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-16LE"
  )
  return(raw_block)
}

# function remove_qualtrics_artifacts(block, block_id)
remove_qualtrics_artifacts <- function(block, block_id) {

  .rows <- -c(1, 3) # to remove first and third rows
  .cols <- c( # only keep columns relevant to analysis
    grep(IP_TAG, block), 
    grep(QUALTRICS_EXPORT_TAG, block), 
    grep(IMAGE_BLOCK_TAG, block)
  )

  block <- block[
    .rows,
    .cols
  ]
  
  # remove extension and appended characters in column names
  block[1, ] <- lapply(block[1, ], sub_qualtrics_image_string)
  
  # drop automated column indices and set colnames
  rownames(block) <- NULL # reset named row indices
  colnames(block) <- block[1, ]
  block <- block[-1, ]
  
  # filter for block_id (i.e., "02") in imageBlock
  block <- dplyr::filter(block, imageBlock == block_id)

  # rename `IP Address` for access convenience
  block <- plyr::rename(block, replace = c(IP_NAME = IP_RENAME))

  return(block)
  
}

# function sub_qualtrics_image_string(str)
sub_qualtrics_image_string <- function(str) {
  .pattern <- sprintf(".jpg - \\d+%s", QUALTRICS_EXPORT_TAG)
  .replacement <- ""
  gsub(.pattern,.replacement, str)
}
