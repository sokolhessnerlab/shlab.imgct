#' Remove Qualtrics artifacts from a 
#' 
#'
#'
#'

# TODO finish this function

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

# TODO make private

# function sub_qualtrics_image_string(str)
sub_qualtrics_image_string <- function(str) {
  .pattern <- sprintf(".jpg - \\d+%s", QUALTRICS_EXPORT_TAG)
  .replacement <- ""
  gsub(.pattern,.replacement, str)
}
