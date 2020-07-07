#' Validate All Participant Responses In All Blocks
#'
#' \code{validate_all_participants} validates a sequence of scrubbed (clean) blocks of 
#' participant response data against a validation key. The 
#'
#' @param path String path to the directory containing clean block responses,
#' keys and output for validated participant data.
#' @param key_filename Name of the validation key TXT file with extension.
#' Defaults to "validation_key.txt".
#'
#' @return A single master validation dataframe with participants and their
#' validation responses, as well as a count of valid responses.
#'
#' @examples
#' validate_all_participants("/path/to/data", "validation_key.txt")
#'
#' @export
validate_all_participants <- function(path, key_filename = "validation_key.txt") {

  validation_key <- shlab.imgct::load_key(path, key_filename)
  key_names <- names(validation_key) # filenames of the validation images

  all_clean_blocks <- shlab.imgct::load_all_clean_blocks(path)

  validation_list <- list()
  index <- 1

  for (clean_block in all_clean_blocks) {

    validation_list[[index]] <- shlab.imgct::validate_block_participants(
      clean_block,
      validation_key
    )

    index <- index + 1

  }

  all_participant_validations <- dplyr::bind_rows(validation_list)

  readr::write_tsv(
    all_participant_validations, 
    file.path(
      path, 
      "results",
      "all_participant_validations.tsv"
    ), 
    append = FALSE, 
    col_names = TRUE
  )

  return(all_participant_validations)

}
