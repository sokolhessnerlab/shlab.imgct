#' Remove Qualtrics artifacts from a 
#' 
#' \code{remove_qualtrics_artifacts} removes artifacts that are embedded in the raw data exports from Qualtrics.
#'
#' @param block
#' @param block_id
#'
#' @return Return block as dataframe without qualtrics artifacts.
#'
#' @examples
#'
#' @export
remove_qualtrics_artifacts <- function(block, block_id) {

  library("dplyr")
  library("plyr")

  .rows <- -c(1, 3)
  .cols <- c(
    grep("IP Address", block), # IP Address tag find
    grep("Q", block), # question tag find
    grep("imageBlock", block) # temporary...
  )

  block <- block[
    .rows,
    .cols
  ]
  
  # remove file name extension from image names and appended characters in columns
  block[1, ] <- lapply(block[1, ], sub_qualtrics_image_string)
  
  # drop automated column indices and set colnames
  rownames(block) <- NULL # reset named row indices
  colnames(block) <- block[1, ]
  block <- block[-1, ]
  
  # filter for block_id (i.e., "02") in imageBlock, again temporary
  block <- dplyr::filter(block, imageBlock == block_id)

  # rename `IP Address` for access convenience
  block <- plyr::rename(block, replace = c("IP Address" = "ip_address"))

  return(block)
  
}

#' Scrub qualtrics image string at end of name
sub_qualtrics_image_string <- function(str) {
  .pattern <- sprintf(".jpg - \\d+%s", QUALTRICS_EXPORT_TAG)
  .replacement <- ""
  gsub(.pattern,.replacement, str)
}
