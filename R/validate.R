# validate.R

# -----------------------------------------------------------------------------
# CONSTANTS
# -----------------------------------------------------------------------------

VALIDATION_KEY_FILE = "validation_key.txt"
VALIDATION_KEY_DELIM = "="
VALIDATION_KEY_COL_NAMES = c("key", "value")
VALIDATION_KEY_COL_TYPES = readr::cols(.default = readr::col_character())

CLEAN_BLOCKS_COL_TYPES = readr::cols(.default = readr::col_character())

CLEAN_BLOCKS_DIR_NAME = "clean"
KEYS_DIR_NAME = "keys"
VALID_BLOCKS_DIR_NAME = "valid"

VALIDATION_THRESHOLD = 4 # can be reset by percentage on number of validation key values

# -----------------------------------------------------------------------------
# VALIDATE: BY CLEAN BLOCK, BY ALL CLEAN BLOCKS
# -----------------------------------------------------------------------------

# function validate_clean_block(clean_block, validation_key, threshold)
validate_clean_block <- function(clean_block, 
                                 validation_key, 
                                 threshold = VALIDATION_THRESHOLD) {
  
  # pull keys and expand validation key by # participant rows in clean block
  keys <- names(validation_key)
  expanded_validation_key <- uncount(validation_key, nrow(clean_block))
  
  bool_matrix = (expanded_validation_key == clean_block[keys])
  validated_block <- clean_block %>% 
                  mutate(count_valid = rowSums(bool_matrix)) %>% 
                  select(-one_of(keys)) %>% # drop validation image columns
                  filter(count_valid >= threshold)
  
  return(validated_block)
  
}

# function validate_all_clean_blocks(path, threshold)
validate_all_clean_blocks <- function(path, threshold = VALIDATION_THRESHOLD) {
  
  # TODO: error if clean blocks or validation key do not exist
  # TODO: error if threshold is out of bounds of validation key
  
  # paths to data, extendend file paths for clean, keys, valid
  path_to_clean_blocks <- file.path(path, CLEAN_BLOCKS_DIR_NAME)
  path_to_keys <- file.path(path, KEYS_DIR_NAME)
  path_to_valid <- file.path(path, VALID_BLOCKS_DIR_NAME)
  
  validation_key <- load_validation_key(path_to_keys)
  
  filenames <- list.files(
    path = path_to_clean_blocks, 
    pattern = "*.tsv",
    full.names = FALSE # maintain full path/to/file string
  )
  
  for (fn in filenames) {

    path_to_clean_block <- file.path(path_to_clean_blocks, fn)
    clean_block <- load_clean_block(path_to_clean_block)
    validated_block <- validate_clean_block(clean_block, validation_key, threshold)

    readr::write_tsv(
      validated_block, 
      file.path(
        path_to_valid, 
        stringr::str_replace(fn, CLEAN_BLOCKS_DIR_NAME, VALID_BLOCKS_DIR_NAME)
      ), 
      append = FALSE, 
      col_names = TRUE
    )
    
  }
  
}