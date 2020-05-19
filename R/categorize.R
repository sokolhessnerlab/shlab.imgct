#' Categorize Function
#'
#' \code{categorize} loads, pivots, and combines valid blocks
#' of image categorization task responses, then associates
#' participant response counts to categories for each
#' image.
#'
#' @param path The path relative to working directory that holds data in
#' designated "valid" and "keys" directories.
#' @param filename The name of the output file that will be written after
#' categorization is finished.
#
#' @examples
#'
#' @export
categorize <- function(path, filename = "categorized.tsv") {

  path_to_valid <- file.path(path, "valid")
  path_to_categorized <- file.path(path, "categorized")
  path_to_keys <- file.path(path, "keys")
  
  c_key <- shlab.imgct::load_txt_key(
    file.path(path_to_keys, "categorization_key.txt"), 
    key_template = "CATEGORIZATION"
  )
  cat_id_list <- names(c_key)
  cat_name_list <- c_key %>%
    unlist(., use.names = FALSE)

  filenames <- list.files(
    path = path_to_valid, 
    pattern = "*.tsv",
    full.names = FALSE # maintain full path/to/file string
  )

  df_list <- list()
  index <- 1

  for (fn in filenames) {

  	block <- shlab.imgct::load_block(
  		file.path(path_to_valid, fn),
  		block_template = "VALID"
  	)

    counted_block_df <- .count_categories(block, cat_id_list)
      
		df_list[[index]] <- counted_block_df
		index <- index + 1

	}

	# Now bind rows of df_list
	df_bind <- dplyr::bind_rows(df_list)

  # Rename columns with textual key, expect image_id in first column
  df_bind <- df_bind %>% 
    dplyr::rename_at(vars(-c(1)), ~cat_name_list)

  .write(df_bind, path_to_categorized, filename)

	return(df_bind)

}

#' \code{.count_categories} uses a combination of long and wide pivots to
#' provide counted category responses for a given image block.
#'
#' @param block_df A dataframe from loading a clean, validated set of responses
#' to images.
#' @param cat_id_list A list of the identiers used for categorization in the
#' given task.
.count_categories <- function(block_df, cat_id_list) {

  # pivot_longer and group by image_id to count category_id choices per image
  long_block_df <- block_df %>%
    tidyr::pivot_longer(
      cols = -c("participantCode", "count_valid"), 
      names_to = "image_id", 
      values_to = "category_id", 
      values_drop_na = FALSE
    ) %>%
    dplyr::group_by(image_id) %>%
    dplyr::count(category_id) %>%
    dplyr::mutate(
      category_id = replace(category_id, !(category_id %in% cat_id_list), NA)
    )

  # pivot_wider to organize count of each category_id choice by image_id
  counted_block_df <- long_block_df %>% 
    tidyr::pivot_wider(
      id_cols = image_id, 
      names_from = category_id, 
      values_from = n, 
      values_fill = list(n = 0)
    ) %>%
    dplyr::select(image_id, cat_id_list)

  return(counted_block_df)
   
}

#' \code{.write} writes a dataframe to the absolute path composed
#' of path and filename.
#'
#' @param df The dataframe to write to file.
#' @param path The path to the directory in which the file will be written.
#' @param filename The filename of the file that will be written. Defaults to
#' "categorized.tsv".
.write <- function(df, path, filename = "categorized.tsv") {
  readr::write_tsv(
    df,
    file.path(path, filename),
    append = FALSE,
    col_names = TRUE
  )
}
