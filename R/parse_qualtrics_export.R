#' Parse Qualtrics export survey response data
#' 
#' \code{parse_qualtrics_export} removes artifacts that are embedded in the raw data exports from Qualtrics.
#'
#' @param path_to_export A path to the exported survey response data from qualtrics.
#' @param path_to_blocks A path to the directory containing ordered image block lists
#' @param path_to_raw A path to the directory containing individual raw
#' response blocks
#' @param qualtrics_tag A string literal matching the qualtrics tag that was assigned to naming by Qualtrics that
#' should be removed for analysis. Defaults to "_Q10" for now.
#'
#' @return Returns message of success; saves parsed and mutated raw data blocks
#' with correct image name headers.
#'
#' @examples
#' parse_qualtrics_export(path_to_export, path_to_blocks, path_to_raw)
#'
#' @export
parse_qualtrics_export <- function(path_to_export, path_to_blocks, path_to_raw, qualtrics_tag = "_Q10") {

  exported_df <- read.delim(
    path_to_export,
    header = TRUE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    fileEncoding = "UTF-16LE"
  )

  row_drops <- c(2) # secondary headers from Qualtrics must be dropped

  exported_df <- exported_df %>% 
    dplyr::filter(!row_number() %in% row_drops) %>%
    dplyr::filter(Finished == 1) %>%
    dplyr::select(contains(qualtrics_tag), "imageBlock", "participantCode") %>%
    tibble::column_to_rownames(var = "participantCode")

  blocks <- list.files(
    path = path_to_blocks,
    pattern = "*.txt",
    full.names = FALSE
  )

  for (block in blocks) {

    block_id <- stringr::str_extract(block, "\\d+")
    images <- readr::read_lines(file.path(path_to_blocks, block))

    block_df <- exported_df %>%
      tibble::rownames_to_column("participantCode") %>%
      dplyr::filter(imageBlock == block_id) %>%
      dplyr::select(-c(imageBlock)) %>%
      `names<-`(c("participantCode", images))

    readr::write_tsv(
      block_df, 
      file.path(
        path_to_raw, 
        stringr::str_c("raw_", block_id, ".tsv")
      ),
      na = "NA",
      append = FALSE,
      col_names = TRUE
    )

  }

  return(exported_df)

}


