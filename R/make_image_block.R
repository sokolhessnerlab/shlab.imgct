#' Make Image Blocks From Dirs Function
#'
#' \code{make_image_blocks_from_dirs} Creates and writes a TXT blocks of image names.
#'    Notably, this function uses a list of lists of image files from
#'    databases to construct an equally but randomly allocated list of images
#'    from each in a series of equally sized blocks.
#'
#' @param dbs_path The path relative to working directory of database lists of
#' images.
#' @param validation_path The path relative to working directory of validation
#' image files.
#' @param output_path The path relative to working directory of output block
#' TXT files.
#' @param block_size The number of images per block.
#' 
#' @return List of image file names with prefix database identifiers.
#'
#' @examples
#' make_image_list_from_dirs("imgct/images/databases",
#'  "imgct/images/validation",
#'  "imgct/images/blocks",
#'  300)
#'
#' @export
make_image_blocks_from_dirs <- function(dbs_path, validation_path, output_path, block_size) {

  # list names of contents in directory paths
  db_list <- list.files(dbs_path)
  valid_list <- list.files(validation_path)

  # initialize objects
  img_total <- 0
  db_alloc <- list()
  db_imgs <- list()

  # compute database allocations as proportions and add image filename list
  # to list
  for (db in db_list) {

    db_dir <- file.path(dbs_path, db)
    imgs <- list.files(db_dir)

    db_alloc[[db]] <- length(imgs)

    db_imgs[[db]] <- lapply(imgs, 
                            function(i) paste0(toupper(db), "_", stringr::str_replace_all(tolower(i), " ", "_")))

    img_total <- img_total + db_alloc[[db]]
  } 

  db_alloc <- lapply(db_alloc, function(x) x / img_total)

  # round up to number of blocks
  num_blocks <- ceiling(img_total / block_size)

  # create blocks
  for (block in 1:num_blocks) {

    # sample from list of db image lists without replacement and with
    # weighting according to db_alloc
    samp <- sample(unlist(db_imgs), size = block_size, replace = FALSE,
          prob = rep(unlist(db_alloc), times = sapply(db_imgs, length)))
    block_imgs <- sample(samp)

    for (db in db_list) {
      # remove sampled images from database lists and reassign
      imgs <- db_imgs[[db]]
      db_imgs[[db]] <- imgs[!(imgs %in% unique(samp))]
    }

    # append and randomize placement of validation images
    block_list <- sample(unlist(append(samp, valid_list)))

    # write the list to TXT file with associated block number
    write.table(block_list, 
                file.path(output_path, stringr::str_c("block_", stringr::str_pad(block, 1, pad = "0"), ".txt")), 
                row.names = FALSE, col.names = FALSE, quote = FALSE)

  }

}

