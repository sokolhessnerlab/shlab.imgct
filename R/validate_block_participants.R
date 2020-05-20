#' Validate All Participant Responses In One Block
#'
#' \code{validate_block_participants}
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

  participant_validations <- clean_block %>%
    dplyr::select(participantCode, one_of(key_names)) %>%
    dplyr::mutate(total_valid = rowSums(boolean_matches))

  return(participant_validations)
}
