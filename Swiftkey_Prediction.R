#  ----------------------------------------------------------------------------
#  DATA SCIENCE CAPSTONE
#  Data Exploration Week 3
#  File: Swiftkey_Prediction.R
#  (c) 2016 - Enrique Pérez Herrero
#  GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
#  Enrique Pérez Herrero
#  21/Mar/2016
#  ----------------------------------------------------------------------------

# 0. CLEAR ENVIRONMENT AND LOAD PACKAGES

rm(list = ls(all = TRUE))
gc(reset = TRUE)

library(quanteda)
library(wordcloud)
library(ggplot2)
library(stringr)

source("Swiftkey_Data_Access_Functions.R")

# 1. PARAMETERS

AVAILABLE_LANGUAGES
LANGUAGE <- AVAILABLE_LANGUAGES[1]   #english
SAMPLE_SIZE <- c(5000,  # Number of lines: blogs
                 5000,  # news
                 0   # twitter
                 )
CREATE_SAMPLES <- FALSE
# Unknown word marker
UNK <- "UNK"
# App directory
app_dir <- "shiny/data/"


# 2. ACCESS FILES DATA FRAME

files_list <- get_files_list(LANGUAGE)
print(files_list)


# 3. SAMPLE TEXT FILES

set.seed(pi)
if(CREATE_SAMPLES){
  mapply(function(x, y) sample_text_file(x, size = y),
         files_list$FILE_PATH,
         files_list$SAMPLE_SIZE)
  cat(paste("Sample files directory:", sample_dir(LANGUAGE), "\n"))
}


# 4. CREATE SAMPLE FILES CORPUS

crps <- corpus(textfile(files_list$SAMPLE_FILE_PATH))

# 4.1 VIEW CORPUS

summary(crps)
crps$settings
crps$metadata

# 4.  LOAD DICTIONARY:
# cracklib-small included in ubuntu 15.10 distribution

cracklib_dic <- load_dictionary()

clean_input <- function(my_sentence){
  # Remove double spaces
  my_sentence <- gsub(" +", " ", my_sentence)
  # Convert to lower case
  my_sentence <- tolower(my_sentence)
  return(my_sentence)
}

count_words <- function(my_sentence){
  my_sentence <- clean_input(my_sentence)
  sapply(strsplit(my_sentence, " "), length)
}

in_dictionary <- function(my_word){
  answer <- cracklib_dic[cracklib_dic == my_word]
  ifelse(length(answer), answer, "UNK")
}


# 5. ALL SAMPLED CORPUS n-grams FREQUENCIES

get_ngram_freq <- function(my_corpus, ngrams = 1) {
  # my_features <- cracklib_dic
  if(ngrams == 1){
    my_features = cracklib_dic
  }
  my_dfm <-
    dfm(
      my_corpus,
      ngrams = ngrams,
      removeTwitter = TRUE,
      removeNumbers = TRUE,
      what = "fastestword",
      verbose = TRUE,
      language = "english" #,
      #keptFeatures = my_features
    )
  df_freq <- sort(colSums(my_dfm), decreasing = TRUE)
  df_freq  <- data.frame(names(df_freq), df_freq)
  names(df_freq) <- c("words", "value")
  df_freq$words <- gsub("_", " ", rownames(df_freq))
  rownames(df_freq) <- c(1:nrow(df_freq))
  return(df_freq)
}


ggplot_ngram <- function(my_corpus, ngrams = 1, num = 30) {
  df <- data.frame(head(get_ngram_freq(my_corpus, ngrams = ngrams), num))
  my_color <- brewer.pal(min(8, max(3, ngrams)), "Dark2")[ngrams]
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


# 6 PLOT WORD CLOUD
plot_ngram_cloud <- function(my_corpus,
                             ngrams = 1,
                             max_words = 100,
                             palette = brewer.pal(6, "Dark2")) {
  my_dfm <-
    dfm(
      my_corpus,
      ngrams = ngrams,
      removeTwitter = TRUE,
      verbose = FALSE
    )
  plot(my_dfm, max.words = max_words, colors = palette)
}

plot_ngram_cloud(crps, 1)


# 7 GET N-GRAMS AND COMPACT WITH DICTIONARY



clean_input <- function(my_sentence){
  # Remove double spaces
  my_sentence <- gsub(" +", " ", my_sentence)
  # Convert to lower case
  my_sentence <- tolower(my_sentence)
  return(my_sentence)
}

count_words <- function(my_sentence){
  my_sentence <- clean_input(my_sentence)
  sapply(strsplit(my_sentence, " "), length)
}

in_dictionary <- function(my_word){
  my_word <- tolower(my_word)
  answer <- cracklib_dic[cracklib_dic == my_word]
  ifelse(length(answer), answer, "UNK")
}

system.time({
x <- in_dictionary("fooft")
})
x

# ----------------------------------------------------------------------
get_prediction_ngrams <- function(my_sentence, ngram = 4){
  my_sentence <- clean_input(my_sentence)
  num_words <- count_words(my_sentence)
  num_words <- min(ngram, num_words)
  col_ngrams <- word(my_sentence, -1:-num_words, -1)
  return(c("ngram" = col_ngrams))
}



# BIGRAM ----------------------------------------------------------------
prob_last_word <- function(my_sentence, ngram = 2){
  answer <- 0
  my_sentence <- clean_input(my_sentence)
  if(count_words(my_sentence)>= ngram){
    #ngram_words <- word(my_sentence, -1:-4, -1)
    ngram_words <- word(my_sentence, -ngram, -1)
    answer <- bi_gram[bi_gram$words == ngram_words,]
    answer$prob <- 
      answer$value / uni_gram[uni_gram$words == answer$sentence, ]$value
    answer <- ifelse(length(answer$prob) == 0, 0, answer$prob)
  }
  return(answer)
}


# UNIGRAM  ----------------------------------------------------------------

prob_unigram <- function(my_sentence, ngram = 1){
  answer <- 0
  my_sentence <- clean_input(my_sentence)
  if(count_words(my_sentence) >= ngram){
    ngram_words <- word(my_sentence, -ngram, -1)
    answer <- uni_gram[uni_gram$words == ngram_words,]
    answer$prob <- 
      answer$value / uni_gram_size
    answer <- ifelse(length(answer$prob) == 0, 0, answer$prob)
  }
  return(answer)
}

test_phrase <- "Go on a romantic date at the beach"
test_phrase <- "I'd live and I'd give"

word(test_phrase, -1:-4, -1)
lapply(c(4:1), FUN = function(x) word(test_phrase, -x, 4))

prob_last_word(test_phrase)

prob_last_word("I'm")
prob_unigram(test_phrase)



# QUIZ 3 ######################################################################
source("docs/Quiz3.R")


find_answer <- paste(quiz3.questions[6], quiz3.answers[[6]])
sapply(find_answer, prob_last_word)
sapply(find_answer, prob_unigram)


predict_word <- function(my_sentence){
  my_sentence <- tolower(my_sentence)
  my_sentence <- word(my_sentence, -3, -1)
  return(my_sentence)
}

test <- predict_word(quiz2[4])
test
test2 <- paste(word(test, -1), c("defense", "crowd", "players", "referees"))
test2

tri_gram[tri_gram$sentence == word(test, 2, 3),]
tetra_gram[tetra_gram$sentence == test,]
#View(bi_gram)

bi_gram[bi_gram$words == test2[1], ]
bi_gram[bi_gram$words == test2[2], ]
bi_gram[bi_gram$words == test2[3], ]
bi_gram[bi_gram$words == test2[4], ]


# ==========================
sapply(tokenize(crps, removePunct = TRUE, removeNumbers = TRUE)[[1]], in_dictionary)

system.time({
  tkz <- tokenize(
    toLower(crps),
    ngrams = 1,
    removePunct = TRUE,
    removeNumbers = TRUE,
    removeTwitter = TRUE,
    verbose = TRUE
  )
  
})

system.time({
tkz[[1]] <- sapply(tkz[[1]], in_dictionary)
})

tkz[[1]]

tkz2 <- quanteda::ngrams(tkz, n = 2, concatenator = " ")
my_dfm <-  dfm(tkz2)
df_freq <- sort(colSums(my_dfm), decreasing = TRUE)
df_freq  <- data.frame(names(df_freq), df_freq)
names(df_freq) <- c("words", "value")
View(df_freq)
