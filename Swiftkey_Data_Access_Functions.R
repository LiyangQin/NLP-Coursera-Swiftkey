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

# Sample text file located at file_path
# also creates sample directory if needed
sample_text_file <- function(file_path,
                             encode = "UTF-8",
                             size = SAMPLE_SIZE,
                             lang = LANGUAGE){
  doc <- readLines(file_path, encoding = encode)
  doc <- sample(doc, SAMPLE_SIZE)
  if(!dir.exists(sample_dir(lang))){
    dir.create(sample_dir(lang))
    print(paste("Created directory:", sample_dir(lang)))
  }
  sample_file_name <- paste0("sample.", basename(file_path))
  sample_file_path <- paste0(sample_dir(lang), "/", sample_file_name)
  print(paste("Saving sample file:", sample_file_path, ", with", size, "lines"))
  writeLines(doc, sample_file_path)
}