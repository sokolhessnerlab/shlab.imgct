#' Clean Function
#'
#' \code{clean} scrubs a sequenctial list of Qualtrics data export files
#' detailing participant category responses to images within a directory of
#' image blocks.
#'
#' @param path The path relative to working directory that holds data in
#' designated "raw" directory.
#'
#' @examples
#' clean("../mounts/imgct/data/5_category")
#'
#' @export

# -----------------------------------------------------------------------------
# CLEAN
# -----------------------------------------------------------------------------

# function clean(path)
clean <- function(path) {
  
  path_raw = file.path(path, "raw")
  path_clean = file.path(path, "clean")
  
  filenames <- list.files(
    path = path_raw, 
    pattern = "*.tsv",
    full.names = FALSE
  )

  .lower <- 0
  .upper <- 0
  for (fn in filenames) {
    print(fn)
    
    block <- shlab.imgct::load_block(file.path(path_raw, fn), "RAW")
    block <- shlab.imgct::remove_qualtrics_artifacts(block)

    # reset lower bound relative to previous upper bound, then
    # reset upper bound to total rows extracted thus far
    .lower <- .upper + 1
    .upper <- nrow(block)

    block <- dplyr::slice(block, .lower:.upper)
    
    write.table(
      block,
      file = file.path(
        path_clean,
        str_replace(fn, RAW_BLOCKS_DIR_NAME, CLEAN_BLOCKS_DIR_NAME)
      ),
      sep = "\t",
      append = FALSE, # remove file if already exists and replace
      col.names = TRUE,
      row.names = FALSE # remove automated indices as row names
    )
    
  }
  
}
