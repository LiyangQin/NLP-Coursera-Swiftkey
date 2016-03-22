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
library(markovchain)
library(tm)

source("Swiftkey_Data_Access_Functions.R")

# 1. PARAMETERS

AVAILABLE_LANGUAGES
LANGUAGE <- AVAILABLE_LANGUAGES[1]   #english
SAMPLE_SIZE <- c(2000, 2000, 50)
CREATE_SAMPLES <- FALSE

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
print(files_list)


# 3. SAMPLE TEXT FILES

set.seed(pi)
if(CREATE_SAMPLES){
  mapply(function(x, y) sample_text_file(x, size = y) ,
         files_list$FILE_PATH, files_list$SAMPLE_SIZE)
  print(paste("Sample files directory:", sample_dir(LANGUAGE)))
}


# 3. CREATE SAMPLE FILES CORPUS

crps <- corpus(textfile(files_list$SAMPLE_FILE_PATH))

# 3.1 VIEW CORPUS

summary(crps)
crps$metadata

# 6. ALL SAMPLED CORPUS n-grams FRECUENCIES

get_ngram_freq <- function(my_corpus, ngrams = 1) {
  my_dfm <-
    dfm(
      my_corpus,
      ngrams = ngrams,
      removeTwitter = TRUE,
      verbose = FALSE
    )
  df_freq <- sort(colSums(my_dfm), decreasing = TRUE)
  df_freq  <- data.frame(names(df_freq), df_freq)
  names(df_freq) <- c("words", "value")
  df_freq$words <- gsub("_", " ", rownames(df_freq))
  rownames(df_freq) <- c(1:nrow(df_freq))
  return(df_freq)
}

View(get_ngram_freq(crps, ngrams = 3))


ggplot_ngram <- function(my_corpus, ngrams = 1, num = 30){
  df <- data.frame(head(get_ngram_freq(my_corpus, ngrams = ngrams), num))
  my_color <- brewer.pal(8, "Dark2")[ngrams]
  ggplot(df, aes(reorder(words, value), value)) +   
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


# 6.1 PLOT WORD CLOUD
plot_ngram_cloud <- function(my_corpus, ngrams = 1) {
  my_dfm <-
    dfm(
      my_corpus,
      ngrams = ngrams,
      removeTwitter = TRUE,
      verbose = FALSE
    )
  plot(my_dfm, max.words = 100, colors = brewer.pal(6, "Dark2"))
}

plot_ngram_cloud(crps, 1)


tri_gram <- get_ngram_freq(crps, ngram = 3)

View(tri_gram)

next_word <- function(my_text){
  tri_gram <- get_ngram_freq(crps, ngram = 3)
  my_text <- tolower(my_text)
  answer <- tri_gram[grep(paste0("^", my_text), tri_gram$words), ]
  return(answer)
}

next_word("This is")


head(model$estimate)
tail(model$estimate)

x <- predict(model$estimate, newdata = "this_is_a")


