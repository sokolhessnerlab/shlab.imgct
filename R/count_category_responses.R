#' Count Category Responses By Image
#'
#' \code{count_category_responses} uses a combination of long and wide pivots to
#' provide counted category responses for a given image block.
#'
#' @param block A dataframe from loading a clean set of responses
#' to images, or passed in from a parent function.
#' @param category_ids A list of the identiers used for categorization in the
#' given task.
#'
#' @return A dataframe with the block's images and corresponding category
#' counts.
#'
#' @examples
#' count_category_responses(block, c("1", "2", "3", "4", "5"))
#'
#' @export
count_category_responses <- function(block, category_ids) {

  # pivot_longer and group by image_id to count category_id choices per image
  long_block <- block %>%
    tidyr::pivot_longer(
      cols = -c("participant_id"), 
      names_to = "image_id", 
      values_to = "category_id", 
      values_drop_na = FALSE
    ) %>%
    dplyr::group_by(image_id) %>%
    dplyr::count(category_id) %>%
    dplyr::mutate(
      category_id = replace(category_id, !(category_id %in% category_ids), NA)
    )

  # pivot_wider to organize count of each category_id choice by image_id
  counted_block <- long_block %>% 
    tidyr::pivot_wider(
      id_cols = image_id, 
      names_from = category_id, 
      values_from = n, 
      values_fill = list(n = 0)
    ) %>%
    dplyr::select(image_id, category_ids)

  return(counted_block)
   
}


