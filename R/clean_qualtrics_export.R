#' Clean Qualtrics Response Data
#' 
#' \code{clean_qualtrics_tsv} removes artifacts that are 
#' embedded in the raw data exports from Qualtrics.
#'
#' @param path A path to data directories for project.
#' @param filename The name of the exported file from Qualtrics. Note: the
#' file must be a TSV, and the export options must prepare values as numeric
#' that were specially coded in the study.
#' @param qualtrics_tag A string literal matching the qualtrics tag that was
#' assigned to naming by Qualtrics that should be removed for analysis. Defaults 
#' to "_Q10".
#' @param remove_participant_code A boolean to remove the MTurk-created
#' participantCode. Useful if wanting to obfuscate participants. Defaults to
#' TRUE, but change to FALSE if needing to reference raw codes in analysis.
#'
#' @return Success message with reference to the path cleaned blocks were saved
#' to.
#'
#' @examples
#' clean_qualtrics_export(path, filename)
#'
#' @export
clean_qualtrics_export <- function(path, 
                                   filename = "qualtrics.tsv", 
                                   qualtrics_tag = "_Q10",
                                   remove_participant_code = TRUE) {

  qualtrics_export <- load_qualtrics_tsv(path, filename)
  qualtrics_parsed <- parse_qualtrics_export(qualtrics_export, 
                                             qualtrics_tag,
                                             remove_participant_code)

  path_to_blocks <- file.path(path, "blocks")
  blocks <- list.files(
    path = path_to_blocks,
    pattern = "*.txt",
    full.names = FALSE
  )

  # Each block of image names must be reapplied as column headers for 
  # each corresponding set of participants and responses in sequence.
  for (block in blocks) {

    block_id <- stringr::str_extract(block, "\\d+")
    images <- readr::read_lines(file.path(path_to_blocks, block))

    clean_block <- qualtrics_parsed %>%
      #tibble::rownames_to_column("participant_id") %>%
      dplyr::filter(imageBlock == block_id) %>%
      dplyr::select(-c("imageBlock")) %>%
      `names<-`(c("participant_id", images))

    readr::write_tsv(
      clean_block, 
      file.path(
        path, 
        "clean",
        stringr::str_c("clean_", block_id, ".tsv")
      ),
      na = "NA",
      append = FALSE,
      col_names = TRUE
    )

  }

  return(
    paste(
      "Success! Your clean blocks were saved to ", 
      file.path(path, "clean")
    )
  )

}
