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

  # TODO: error if valid blocks or validation key do not exist
  # TODO: error if threshold is out of bounds of validation key
  path_valid = file.path(path, "valid")
  path_categorized = file.path(path, "categorized")
  path_keys = file.path(path, "keys")
  
  c_key <- shlab.imgct::load_txt_key(
    file.path(path_keys, "categorization_key.txt"), 
    key_template = "CATEGORIZATION"
  )
  c_keys_by_name = names(c_key)

	# TODO: handle duplicates
	#keys <- names(category_key) # to rename category column names with actual name

	# pivot_longer and group by image_id to count category_id choices per image
	block <- block %>%
		tidyr::pivot_longer(
			cols = -c("ip_address", "count_valid"), 
			names_to = "image_id", 
			values_to = "category_id", 
			values_drop_na = TRUE # ...maybe
		) %>%
		group_by(image_id) %>%
		count(category_id)

	# pivot_wider to organize count of each category_id choice by image_id
	block <- block %>% 
		tidyr::pivot_wider(
			id_cols = image_id, 
			names_from = category_id, 
			values_from = n, 
			values_fill = list(n = 0)
		)

	#? rename columns of counted categories to actual category names
	#category_names = unlist(category_key, use.names = FALSE)
	#names(categorized_block)[names(category_key)] = category_names

	return(block)

}