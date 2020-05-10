#' Categorize Function
#'
#' \code{c} loads, pivots, and combines valid blocks
#' of image categorization task responses, then associates
#' participant response counts to categories for each
#' image.
#'
#' @param path The path relative to working directory that holds data in
#' designated "valid" and "keys" directories.
#
#' @examples
#'
#' @export
categorize <- function(path) {

  path_to_valid = file.path(path, "valid")
  path_to_categorized = file.path(path, "categorized")
  path_to_keys = file.path(path, "keys")
  
  c_key <- shlab.imgct::load_txt_key(
    file.path(path_to_keys, "categorization_key.txt"), 
    key_template = "CATEGORIZATION"
  )
  c_keys_by_code <- names(c_key)

  #TODO: generalize the throwing out edge case values like "1,2" with a na_if
  #statement or a replace statement somewhere, which only keeps the actual
  #category numbers 1, 2, 3, 4, 5...but does this regardless of the task

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

		# pivot_longer and group by image_id to count category_id choices per image
		block <- block %>%
			tidyr::pivot_longer(
				cols = -c("participantCode", "count_valid"), 
				names_to = "image_id", 
				values_to = "category_id", 
				values_drop_na = FALSE # ...maybe
			) %>%
			dplyr::group_by(image_id) %>%
			dplyr::count(category_id) %>%
      dplyr::mutate(category_id = dplyr::na_if(category_id, "1,2"))

		# pivot_wider to organize count of each category_id choice by image_id
		block <- block %>% 
			tidyr::pivot_wider(
				id_cols = image_id, 
				names_from = category_id, 
				values_from = n, 
				values_fill = list(n = 0)
			) %>%
      dplyr::select(image_id, c_keys_by_code)
      
		# add convenience column with block index
    #
		# block <- block %>%
		#	  dplyr::mutate(block_index = index)

		df_list[[index]] <- block
		index <- index + 1

		#? rename columns of counted categories to actual category names
		#category_names = unlist(category_key, use.names = FALSE)
		#names(categorized_block)[names(category_key)] = category_names

	}

	# Now bind rows of df_list
	df_bind <- dplyr::bind_rows(df_list)

  readr::write_tsv(
    df_bind,
    file.path(path_to_categorized, "categorized.tsv"),
    append = FALSE,
    col_names = TRUE
  )

	return(df_bind)

}
