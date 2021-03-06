#' Categorize Function
#'
#' \code{categorize} loads, pivots, and combines valid blocks
#' of image categorization task responses, then associates
#' participant response counts to categories for each
#' image.
#'
#' @param path The path relative to working directory that holds data in
#' designated "valid" and "keys" directories.
#' @param key_filename Name of the categorization key TXT file with extension.
#' Defaults to "categorization_key.txt".
#' @param threshold The minimum number of correct validations participants must have
#' made to have their responses considered for categorization. Defaults to 0.
#'
#' @return A categorized dataframe of images for a given validation
#' threshold. The dataframe is saved to file with a reference to valdiation
#' threshold, such that the output filename takes the form
#' "categorized_X_valid.tsv", where X is the minimum number of validations
#' expected correct for the task.
#'
#' @examples
#' categorize("path/to/data", threshold = 3)
#'
#' @export
categorize <- function(path,  
                       key_filename = "categorization_key.txt",
                       threshold = 0) {

  categorization_key <- shlab.imgct::load_key(path, key_filename)
  key_ids <- names(categorization_key)
  key_names <- categorization_key %>% 
    unlist(., use.names = FALSE)

  all_clean_blocks <- shlab.imgct::load_all_clean_blocks(path)
  all_participant_validations <- shlab.imgct::load_all_participant_validations(path)
  validation_cols <- names(
    all_participant_validations %>% dplyr::select(-c("participant_id")) 
  )

  categorization_list <- list()
  index <- 1

  for (clean_block in all_clean_blocks) {

    block_above_threshold <- clean_block %>%
      dplyr::left_join(
        select(
          all_participant_validations, 
          c("participant_id", "total_valid")
        ), 
        by = "participant_id"
      ) %>% 
      dplyr::filter(total_valid >= threshold) %>%
      dplyr::select(-validation_cols)

    categorization_list[[index]] <- count_category_responses(block_above_threshold, key_ids)

    index <- index + 1

	}

	# Now bind by rows and rename columns with key, 
  # with image_id in first column and then ungroup to flatten
	categorized <- dplyr::bind_rows(categorization_list) %>%
    dplyr::rename_at(vars(-c(1)), ~key_names) %>%
    dplyr::ungroup()

  # Mutate to include number of ratings n_ratings as a total
  # across categories. Consider it the total ratings per image
  # for the given validation threshold
  categorized <- categorized %>%
    dplyr::mutate(
      n_ratings = purrr::reduce(
        dplyr::select(., -image_id), # select category columns to sum across
        `+`
      )
    )

  output_filename <- stringr::str_c(
    "categorized_",
    threshold,
    "_valid.tsv"
  )

  readr::write_tsv(
    categorized,
    file.path(path, "results", output_filename),
    append = FALSE,
    col_names = TRUE
  )

	return(categorized)

}
