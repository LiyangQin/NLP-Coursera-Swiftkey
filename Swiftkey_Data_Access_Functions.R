#  ----------------------------------------------------------------------------
#  COURSERA JHU - DATA SCIENCE CAPSTONE
#  Data Exploration Week 2
#  File: Swiftkey_Data_Access_Functions.R
#  (c) 2016 - Enrique Pérez Herrero
#  GNU GENERAL PUBLIC LICENSE Version 2, June 1991
#  9/Apr/2016
#  ----------------------------------------------------------------------------

# Vector with available languages in the data set
AVAILABLE_LANGUAGES <- c("en_US", "ru_RU", "de_DE", "fi_FI")
names(AVAILABLE_LANGUAGES) <- c("english", "russian", "german", "finnish")
LANGUAGE <- AVAILABLE_LANGUAGES[1]   #english

get_path <- function(lang = LANGUAGE){
  path_to_data <- "data/final"
  paste(path_to_data, lang, sep = "/")
}

get_files_path <- function(lang = LANGUAGE){
  path_to_data <- "data/final/"
  sources <- c("blogs", "news", "twitter")
  paste0(get_path(lang),  "/", lang, ".", sources, ".txt")
}

sample_dir <- function(lang = LANGUAGE){
  paste0(get_path(lang), "/samples")
}

sample_file_name <- function(file_path){
  paste0("sample.", basename(file_path))
}

sample_file_path <-  function(file_path, lang = LANGUAGE){
  paste0(sample_dir(lang), "/", sample_file_name(file_path))
} 

# Sample text file located at file_path
# also creates sample directory if needed
sample_text_file <- function(file_path,
                             encode = "UTF-8",
                             size,
                             lang = LANGUAGE){
  con <- file(file_path, open = "r", encoding = encode)
  doc <- readLines(con, encoding = encode, warn = FALSE)
  close(con)
  doc <- sample(doc, size)
  if(!dir.exists(sample_dir(lang))){
    dir.create(sample_dir(lang))
    cat(paste("Created directory:", sample_dir(lang), "\n"))
  }
  my_sample_file_name <- sample_file_name(file_path)
  my_sample_file_path <- sample_file_path(file_path, lang)
  cat(paste("Saving sample file:", my_sample_file_path, ", with", size, "lines\n"))
  writeLines(doc, my_sample_file_path)
}

get_files_list <- function(lang = LANGUAGE){
  my_files_list <- data.frame(
    get_files_path(lang),
    list.files(get_path(lang), pattern = "*.txt"),
    sample_file_path(get_files_path(lang), lang = lang),
    sample_file_name(get_files_path(lang)),
    SAMPLE_SIZE,
    stringsAsFactors = FALSE
  )
  
  names(my_files_list) <- c("FILE_PATH",
                            "FILE_NAME",
                            "SAMPLE_FILE_PATH",
                            "SAMPLE_FILE_NAME",
                            "SAMPLE_SIZE")
  return(my_files_list)
}

# DICTIONARY
# Stanford English Dictionary:
# http://web.stanford.edu/class/cs106l/assignments/dictionary.txt
# cracklib-small included in ubuntu 15.10 distribution
# https://github.com/cracklib/cracklib/blob/master/src/dicts/cracklib-small

load_dictionary <- function(my_dict = "dic/cracklib-small") {
  cat(paste("Loaded dictionary:", my_dict, "\n"))
  con <- file(my_dict, open = "r", encoding = "UTF-8")
  doc <- readLines(con, encoding = "UTF-8", warn = FALSE)
  close(con)
  return(doc)
}