#' Parse Qualtrics Export Dataframe
#'
#' \code{parse_qualtrics_export}
#'
#' @param qualtrics_export The path string to the clean block to load.
#' @param qualtrics_tag String literal for Qualtrics naming convention needed
#' for parsing. Defaults to "_Q10".
#' @param drop_rows A vector of row numbers to drop. Defaults to `c(2)`.
#'
#' @return parsed_qualtrics_export A dataframe of the parsed qualtrics export
#' data, meaning no artifacts are left behind.
#'
#' @examples
#' parse_qualtrics_export(qualtrics_export)
#'
#' @export
parse_qualtrics_export <- function(qualtrics_export, 
                                   qualtrics_tag = "_Q10", 
                                   drop_rows = c(2)) {
  parsed_qualtrics_export <- qualtrics_export %>% 
    dplyr::filter(!row_number() %in% drop_rows) %>%
    dplyr::filter(Finished == 1) %>%
    dplyr::select(contains(qualtrics_tag), "imageBlock", "participantCode") %>%
    tibble::column_to_rownames(var = "participantCode")
  return(parsed_qualtrics_export)
}
