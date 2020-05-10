#' Parse Qualtrics export survey response data
#' 
#' \code{clean_qualtrics_export} removes artifacts that are embedded in the raw data exports from Qualtrics.
#'
#' @param path A path to data directories for project.
#' @param export_name The name of the exported file from Qualtrics. Note: the
#' file must be a TSV, and the export options must prepare values as numeric
#' that were specially coded in the study.
#' @param qualtrics_tag A string literal matching the qualtrics tag that was assigned to naming by Qualtrics that
#' should be removed for analysis. Defaults to "_Q10" for now.
#'
#' @return Returns parsed data frame established prior to looping and
#' writing individualized response files.
#'
#' @examples
#' clean_qualtrics_export(path, export_name)
#'
#' @export
clean_qualtrics_export <- function(path, 
                                   export_name = "qualtrics_export", 
                                   qualtrics_tag = "_Q10") {

  path_to_raw <- file.path(path, "raw")
  exported_df <- load_export(path, export_name)
  parsed_df <- parse_export(exported_df, qualtrics_tag)

  path_to_blocks <- file.path(path, "blocks")
  blocks <- list.files(
    path = path_to_blocks,
    pattern = "*.txt",
    full.names = FALSE
  )

  for (block in blocks) {

    block_id <- stringr::str_extract(block, "\\d+")
    images <- readr::read_lines(file.path(path_to_blocks, block))

    block_df <- parsed_df %>%
      tibble::rownames_to_column("participantCode") %>%
      dplyr::filter(imageBlock == block_id) %>%
      dplyr::select(-c("imageBlock")) %>%
      `names<-`(c("participantCode", images))

    readr::write_tsv(
      block_df, 
      file.path(
        path_to_raw, 
        stringr::str_c("clean_", block_id, ".tsv")
      ),
      na = "NA",
      append = FALSE,
      col_names = TRUE
    )

  }

  return(exported_df)

}

### load_export(export_path, export_name)
load_export <- function(path, name) {
  export_path <- file.path(
    path,
    stringr::str_c(name, ".tsv")
  )
  exported_df <- read.delim(
    export_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    fileEncoding = "UTF-16LE"
  )
  return(exported_df)
}

### parse_export(exported_df)
parse_export <- function(exported_df, qualtrics_tag = "_Q", drop_rows = c(2)) {
  parsed_df <- exported_df %>% 
    dplyr::filter(!row_number() %in% drop_rows) %>%
    dplyr::filter(Finished == 1) %>%
    dplyr::select(contains(qualtrics_tag), "imageBlock", "participantCode") %>%
    tibble::column_to_rownames(var = "participantCode")
  return(parsed_df)
}
