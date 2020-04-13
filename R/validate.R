#' Validate Function
#'
#' \code{validate} validates a sequence of scrubbed (clean) blocks of data
#' against a validation key.
#'
#' @param path The path relative to working directory that holds data in
#' designated "clean" and "keys" directories.
#' @param threshold The minimum level of validation accuracy for a given
#' participant required to be considered a "valid" point for further analysis. 
#' Defaults to 0, meaning no threshold.
#' @param remove_below_threshold Boolean determining whether participants
#' below validation threshold are removed from output. Defaults to false.
#'
#' @examples
#' validate("../mounts/imgct/data/5_category")
#'
#' @export
validate <- function(path, threshold = 0, remove_below_threshold = FALSE) {

  # TODO: error if clean blocks or validation key do not exist
  # TODO: error if threshold is out of bounds of validation key
  path_clean = file.path(path, "clean")
  path_valid = file.path(path, "valid")
  path_keys = file.path(path, "keys")
  
  v_key <- shlab.imgct::load_txt_key(
    file.path(path_keys, "validation_key.txt"), 
    key_template = "VALIDATION"
  )
  v_keys_by_name = names(v_key)

  filenames <- list.files(
    path = path_clean, 
    pattern = "*.tsv",
    full.names = FALSE # maintain full path/to/file string
  )

  for (fn in filenames) {

    block <- shlab.imgct::load_block(file.path(path_clean, fn))
    v_key_exp <- uncount(v_key, nrow(block))
    bool_block <- (block[v_keys_by_name] == v_key_exp)

    # NOTE: if remove_below_threshold = TRUE...need to do something else
    # if (!remove_below_threshold) {
    #   do something different!
    # }

    block <- block %>% 
              mutate(count_valid = rowSums(bool_block)) %>% 
              select(-one_of(keys)) %>% # drop validation image columns
              filter(count_valid >= threshold)

    readr::write_tsv(
      block, 
      file.path(
        path_valid, 
        stringr::str_replace(fn, "clean", "valid")
      ), 
      append = FALSE, 
      col_names = TRUE
    )

  }
}
