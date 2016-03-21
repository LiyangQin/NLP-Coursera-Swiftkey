#  ----------------------------------------------------------------------------
#  DATA SCIENCE CAPSTONE
#  Data Exploration Week 2
#  File: Swiftkey_Data_Access_Functions.R
#  Enrique Pérez Herrero
#  16/Mar/2016
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
                             size = SAMPLE_SIZE,
                             lang = LANGUAGE){
  con <- file(file_path, open = "r", encoding = encode)
  doc <- readLines(con, encoding = encode)
  close(con)
  doc <- sample(doc, SAMPLE_SIZE)
  if(!dir.exists(sample_dir(lang))){
    dir.create(sample_dir(lang))
    print(paste("Created directory:", sample_dir(lang)))
  }
  my_sample_file_name <- sample_file_name(file_path)
  my_sample_file_path <- sample_file_path(file_path, lang)
  print(paste("Saving sample file:", my_sample_file_path, ", with", size, "lines"))
  writeLines(doc, my_sample_file_path)
}