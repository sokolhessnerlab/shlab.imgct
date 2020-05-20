#' Load All Participant Validations
#'
#' \code{load_all_participant_validations} loads a clean block at a specified full path to the
#'
#' @param path The path string to the parent directory in which the validation
#' file exists. This should be the same parent directory for "clean" block data.
#'
#' @return all_participant_validations A dataframe consisting of all
#' participants and all validation responses with a total number of correct
#' validations.
#'
#' @examples
#' load_all_participant_validations(path)
#'
#' @export
load_all_participant_validations <- function(path) {
  filename <- file.path(path, "all_participant_validations.tsv")
  if (file.exists(filename)) {
    all_participant_validations <- readr::read_tsv(
      filename,
      col_names = TRUE,
      col_types = readr::cols(.default = readr::col_character())
    )
    return(all_participant_validations)
  } else {
    return("The file you are attempting to load does not exist. 
           Please run `validate_all_participants` at the specified path.")
  }
}
