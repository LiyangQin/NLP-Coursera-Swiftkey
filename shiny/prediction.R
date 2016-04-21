#  ----------------------------------------------------------------------------
#  COURSERA JHU - DATA SCIENCE CAPSTONE
#  Prediction algorithm
#  File: prediction.R
#  (c) 2016 - Enrique Pérez Herrero
#  GNU GENERAL PUBLIC LICENSE Version 2, June 1991
#  21/Apr/2016
#  ----------------------------------------------------------------------------

library(quanteda)
library(stringr)

# Unknown word marker
UNK <- "UNK"
# App directory
app_dir_path <- "data/"
app_dic_path <- "dic/cracklib-small"

# 2. LOAD DICTIONARY

load_dictionary <- function(my_dict = app_dic_path) {
  cat(paste("Loaded dictionary:", my_dict, "\n"))
  con <- file(my_dict, open = "r", encoding = "UTF-8")
  doc <- readLines(con, encoding = "UTF-8", warn = FALSE)
  close(con)
  return(doc)
}

cracklib_dic <- load_dictionary()

in_dictionary <- function(my_word){
  answer <- cracklib_dic[cracklib_dic == my_word]
  ifelse(length(answer), answer, UNK)
}


# 3. LOAD N-GRAMS

unigrams <- readRDS(paste0(app_dir_path, "1.ngr"))
bigrams <- readRDS(paste0(app_dir_path, "2.ngr"))
trigrams <- readRDS(paste0(app_dir_path, "3.ngr"))
tetragrams <- readRDS(paste0(app_dir_path, "4.ngr"))

ngrams_names <- c("unigrams", "bigrams", "trigrams", "tetragrams")

# 4. WORD EXTRACTION AND CLEANING

clean_input <- function(my_sentence){
  my_sentence <- toLower(my_sentence)
  # Remove non alphabetic characters
  my_sentence <- gsub("[^[:alpha:] ]", "", my_sentence)
  # Remove duplicated spaces
  my_sentence <- gsub("\\s+", " ", my_sentence)
  # Remove leading or trailing spaces
  # other option is to use str_trim {stringr}
  my_sentence <- gsub("^\\s+|\\s+$", "", my_sentence)
  return(my_sentence)
}

count_words <- function(my_sentence){
  my_sentence <- clean_input(my_sentence)
  sapply(strsplit(my_sentence, " "), length)
}

get_prediction_ngrams <- function(my_sentence, ngram = 3){
  my_sentence <- clean_input(my_sentence)
  num_words <- count_words(my_sentence)
  num_words <- min(ngram, num_words)
  my_sentence <-  word(my_sentence, -num_words, -1)
  my_sentence <- sapply(c(num_words:1),
                        function(x) in_dictionary(word(my_sentence, -x)))
  my_sentence <- paste(my_sentence, collapse = "_")
  col_ngrams <- word(my_sentence, -1:-num_words, -1, sep = fixed("_"))
  ngrams_df <- data.frame(col_ngrams, stringsAsFactors = FALSE)
  colnames(ngrams_df) <- "ngram"
  return(ngrams_df)
}


# 5. Prediction
predict_word <- function(my_sentence){
  my_sentence <- clean_input(my_sentence)
  num_words <- count_words(my_sentence)
  ngrams_df <- get_prediction_ngrams(my_sentence)
  
  if(num_words >= 3){
    prediction <- tetragrams[tetragrams$sentence == ngrams_df$ngram[3], ]
    prediction <- head(prediction, 1)$prediction
    cat(paste("Prediction tetragram: ", prediction, "\n"))
    if(!identical(prediction, character(0))){
      return(prediction)
    }
  }
  
  if(num_words >= 2){
    prediction <- trigrams[trigrams$sentence == ngrams_df$ngram[2], ]
    prediction <- head(prediction, 1)$prediction
    cat(paste("Prediction trigram: ", prediction, "\n"))
    if(!identical(prediction, character(0))){
      return(prediction)
    }
  }
  
  if(num_words >= 1){
    prediction <- bigrams[bigrams$sentence == ngrams_df$ngram[1], ]
    prediction <- head(prediction, 1)$prediction
    cat(paste("Prediction bigram: ", prediction, "\n"))
    if(!identical(prediction, character(0))){
      return(prediction)
    }
  }
  prediction <- "the"
  cat(paste("Prediction unigram: ", prediction, "\n"))
  return(prediction)
}

