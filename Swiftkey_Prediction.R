#  ----------------------------------------------------------------------------
#  DATA SCIENCE CAPSTONE
#  Data Exploration Week 3
#  File: Swiftkey_Prediction.R
#  Enrique Pérez Herrero
#  21/Mar/2016
#  ----------------------------------------------------------------------------

# 0. CLEAR ENVIRONMENT AND LOAD PACKAGES

rm(list = ls(all = TRUE))
gc(reset = TRUE)

library(quanteda)
library(wordcloud)
library(ggplot2)

source("Swiftkey_Data_Access_Functions.R")

# 1. PARAMETERS

AVAILABLE_LANGUAGES
LANGUAGE <- AVAILABLE_LANGUAGES[1]   #english
SAMPLE_SIZE <- c(1000, 1000, 500)

# 2. ACCESS FILES DATA FRAME

files_list <- data.frame(
  get_files_path(LANGUAGE),
  list.files(get_path(LANGUAGE), pattern = "*.txt"),
  sample_file_path(get_files_path(LANGUAGE), lang = LANGUAGE),
  sample_file_name(get_files_path(LANGUAGE)),
  SAMPLE_SIZE,
  stringsAsFactors = FALSE
)

names(files_list) <- c("FILE_PATH",
                       "FILE_NAME",
                       "SAMPLE_FILE_PATH",
                       "SAMPLE_FILE_NAME",
                       "SAMPLE_SIZE")
View(files_list)


# 3. SAMPLE TEXT FILES

set.seed(pi)
mapply(function(x, y) sample_text_file(x, size = y) ,
       files_list$FILE_PATH, files_list$SAMPLE_SIZE)

# Directory for sample files.
sample_dir(LANGUAGE)

# 3. CREATE SAMPLE FILES CORPUS

crps <- corpus(textfile(files_list$SAMPLE_FILE_PATH))

# 3.1 VIEW CORPUS

summary(crps)
crps$metadata

# 6. ALL SAMPLED CORPUS n-grams FRECUENCIES

get_ngram_freq <- function(my_corpus, ngrams = 1){
  my_dfm <- dfm(my_corpus, ngrams = ngrams)
  df_freq <- sort(colSums(my_dfm), decreasing = TRUE)
  df_freq  <- data.frame(df_freq)
  names(df_freq) <- "value"
  df_freq$names <- rownames(df_freq)
  rownames(df_freq) <- c(1:nrow(df_freq))
  return(df_freq)
}

View(get_ngram_freq(crps, ngrams = 3))


View(unigram_freq)

plot(my_dfm, max.words = 100, colors = brewer.pal(6, "Dark2"))

ngrams(crps$documents, n = 2)

ggplot_ngram <- function(my_corpus, ngrams = 1, num = 30){
  df <- data.frame(head(get_ngram_freq(my_corpus, ngrams = ngrams), num))
  my_color <- brewer.pal(8, "Dark2")[ngrams]
  ggplot(df, aes(reorder(names, value), value)) +   
    geom_bar(stat = "identity", fill = my_color) +
    coord_flip() +
    xlab("") +
    ylab("") +
    theme_minimal()
}


ggplot_ngram(crps, ngrams = 1)
ggplot_ngram(crps, ngrams = 2)
ggplot_ngram(crps, ngrams = 3)
ggplot_ngram(crps, ngrams = 4)
