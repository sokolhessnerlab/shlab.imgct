#' Validate All Participant Responses In One Block
#'
#' \code{validate_block_participants} assesses the validation image responses
#' by participants in a given clean image block. The validations are made
#' relative to a validation key, and the function returns a dataframe of
#' validation responses and total number of validations correctly made by
#' participant.
#'
#' @param clean_block A loaded dataframe of participant responses 
#' to a specified block of images. The dataframe must be 
#' scrubbed of Qualtrics or other artifacts prior to usage with this function.
#' @param validation_key A loaded validation key.
#'
#' @return participant_validations The block is returned as a validation block
#' consisting of only validation images and participant responses, plus a count
#' of valid responses per participant.
#'
#' @examples
#' validate_block_participants(clean_block_03, validation_key)
#'
#' @export
validate_block_participants <- function(clean_block, validation_key) {

  expanded_key <- tidyr::uncount(validation_key, nrow(clean_block))

  key_names <- names(validation_key)
  boolean_matches <- (clean_block[key_names] == expanded_key)

  # Evaluate number of correct validation responses by each participant and
  # aggregate into a total_valid column
  participant_validations <- clean_block %>%
    dplyr::select(participant_id, one_of(key_names)) %>%
    dplyr::mutate(total_valid = rowSums(boolean_matches))

  return(participant_validations)
}
