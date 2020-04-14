#' Remove Qualtrics artifacts from a 
#' 
#' \code{remove_qualtrics_artifacts} removes artifacts that are embedded in the raw data exports from Qualtrics.
#'
#' @param block A dataframe of a block of loaded raw data from qualtrics.
#' @param qualtrics_tag A string literal matching the qualtrics tag that was assigned to naming by Qualtrics that
#' should be removed for analysis. Defaults to "_Q10" for now.
#'
#' @return Return block as dataframe without qualtrics artifacts.
#'
#' @examples
#'
#' @export
remove_qualtrics_artifacts <- function(block, qualtrics_tag = "_Q10") {

  .rows <- -c(1, 3)
  .cols <- c(
    grep("IP Address|IPAddress", block[1, ]), # IP Address tag find, either style
    grep(qualtrics_tag, block[1, ]) # question tag find
  )

  block <- block[
    .rows,
    .cols
  ]
  
  # remove file name extension from image names and appended characters in columns
  block[1, ] <- lapply(block[1, ], sub_qualtrics_image_string)
  
  # drop automated column indices and set colnames
  colnames(block) <- block[1, ]
  block <- block[-1, ]

  rownames(block) <- NULL # reset named row indices

  # rename `IP Address` for access convenience
  block <- plyr::rename(block, replace = c("IP Address" = "ip_address"))

  return(block)
  
}

#' Scrub qualtrics image string at end of name
sub_qualtrics_image_string <- function(str, qualtrics_tag = "_Q10") {
  .pattern <- sprintf(".jpg - \\d+%s", qualtrics_tag)
  .replacement <- ""
  gsub(.pattern, .replacement, str)
}
