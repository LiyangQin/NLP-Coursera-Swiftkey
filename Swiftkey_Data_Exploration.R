#  ----------------------------------------------------------------------------
#  DATA SCIENCE CAPSTONE
#  Data Exploration Week 2
#  File: Swiftkey_Data_Exploration.R
#  Enrique Pérez Herrero
#  08/Mar/2016
#  ----------------------------------------------------------------------------

# The first step in building a predictive model for text is understanding the
# distribution and relationship between the words, tokens, and phrases in the
# text. The goal of this task is to understand the basic relationships you 
# observe in the data and prepare to build your first linguistic models.
# 
# Tasks to accomplish
# 
# Exploratory analysis - perform a thorough exploratory analysis of the data,
# understanding the distribution of words and relationship between the words in
# the corpora.
# Understand frequencies of words and word pairs - build figures and tables to
# understand variation in the frequencies of words and word pairs in the data.
# 
# Questions to consider
# 
#  - Some words are more frequent than others - what are the distributions of 
#     word frequencies?
#  - What are the frequencies of 2-grams and 3-grams in the dataset?
#  - How many unique words do you need in a frequency sorted dictionary to cover
#     50% of all word instances in the language? 90%?
#  - How do you evaluate how many of the words come from foreign languages?
#  - Can you think of a way to increase the coverage -- identifying words that
#     may not be in the corpora or using a smaller number of words in the
#     dictionary to cover the same number of phrases?

# 0. CLEAR ENVIRONMENT AND LOAD PACKAGES

rm(list = ls(all = TRUE))
gc(reset = TRUE)

library(tm)
library(readr)
library(wordcloud)
library(RWeka)
library(stringi)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(textcat)

# 0.1 LOAD DATA ACCESS FUNCTIONS

source("Swiftkey_Data_Access_Functions.R")

# 1. PARAMETERS

AVAILABLE_LANGUAGES
LANGUAGE <- AVAILABLE_LANGUAGES[1]   #english
SAMPLE_SIZE <- 1000

# 2. ACCESS FILES

## Examples
file.info(get_files_path(LANGUAGE))
get_path(LANGUAGE)
files_list <- get_files_path(LANGUAGE)
names(files_list) <- list.files(get_path(LANGUAGE), pattern = "*.txt")

files_list


# 3. FILES SUMMARY

# One Megabyte
Mb <- 2**20

# 3.1 Files names
files_name <- as.vector(names(files_list))

# 3.2 Files sizes
files_size <- round(file.info(files_list)$size / Mb)

# 3.3 Number of lines
files_lines <- sapply(files_list,
                      function(x) length(readLines(x, warn = FALSE)))
# 3.4 Test encoding
files_encoding <- sapply(files_list,
                         function(x) guess_encoding(x, n_max = 1000))
files_encoding <- t(files_encoding)

# 3.5 Count words
files_stats <- sapply(files_list,
               function(x) stri_stats_latex(readLines(x, warn = FALSE)))

# 3.4 Create data frame.
files_words <- data.frame(t(files_stats))$Words
files_summary_df <- data.frame(row.names = files_name,
                               files_size,
                               files_lines,
                               files_words,
                               files_encoding)
files_summary_df <- t(files_summary_df)
View(files_summary_df)

# 4. SAMPLE TEXT FILES

set.seed(pi)

sapply(get_files_path(LANGUAGE),
       function(x) sample_text_file(x, size = SAMPLE_SIZE)
       )

# Directory for sample files.
sample_dir(LANGUAGE)

# 3. CREATE SAMPLE FILES CORPUS

crps <- Corpus(DirSource(sample_dir(LANGUAGE), encoding = "UTF-8"),
              readerControl = list(language = LANGUAGE))

# 3.1 Check corpus

inspect(crps)
meta(crps, "id")

# 4. CORPUS TRANSFORMATIONS

corpus_cleaning <- function(my_corpus){
  my_corpus <- tm_map(my_corpus, stripWhitespace)
  my_corpus <- tm_map(my_corpus, removePunctuation)
  my_corpus <- tm_map(my_corpus, content_transformer(tolower))
  my_corpus <- tm_map(my_corpus, removeNumbers)
  return(my_corpus)
}

crps <- corpus_cleaning(crps)

# 5. DISTRIBUTION OF WORD FREQUENCIES

Tokenizer <- function(x, n_gram = 1){
  NGramTokenizer(x, Weka_control(min = n_gram, max = n_gram))
} 

get_corpus_freq <- function(my_corpus, n_gram = 1){
  print("1")
  dtm_matrix <- DocumentTermMatrix(my_corpus,
                control = list(
                         tokenize = function(x) Tokenizer(x, n_gram))
                )
  print("2")
  dtm_matrix <- as.matrix(dtm_matrix)
  frequency <- colSums(dtm_matrix)
  frequency <- sort(frequency, decreasing = TRUE)
  return(frequency)
}

get_corpus_words <- function(my_corpus, n_gram = 1){
  return(names(get_corpus_freq(my_corpus, n_gram)))
}

# 5.1 Display wordclouds

display_wordcloud <-
  function(my_corpus,num = 100,
           n = 1,
           palette = brewer.pal(8, "Dark2")) {
    frequency <- head(get_corpus_freq(my_corpus, n_gram = 1), num)
    words <- names(frequency)
    my_title <- meta(my_corpus, "id")
    wordcloud(words, frequency, colors = palette)
    text(x = 0.5, y = 1, my_title)
  }

layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
display_wordcloud(crps[1])  # blogs
display_wordcloud(crps[2])  # news
display_wordcloud(crps[3])  # twitter


# 5.2 Plot frecuencies by word

freq_df <- data.frame(head(get_corpus_freq(crps[1]), 15),
                      head(get_corpus_freq(crps[2]), 15),
                      head(get_corpus_freq(crps[3]), 15))
names(freq_df) <- list.files(sample_dir(LANGUAGE), pattern = "*.txt")
freq_df$names <- rownames(freq_df)
freq_df <- melt(freq_df, id.vars = 4)

ggplot(freq_df, aes(reorder(names, value), value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("") 


#textcat(head(names(get_corpus_freq(crps)), 10))
crps


head(get_corpus_freq(crps, n_gram = 1), 10)
head(get_corpus_freq(crps, n_gram = 2), 10)
head(get_corpus_freq(crps, n_gram = 3), 10)
head(get_corpus_freq(crps, n_gram = 4), 10)

# 6. ALL SAMPLED CORPUS n-grams FRECUENCIES 

ggplot_ngram <- function(my_corpus, n = 1, num = 30){
  df <- data.frame(head(get_corpus_freq(my_corpus, n_gram = n), num))
  names(df) <- "value"
  df$names <- rownames(df)
  my_color <- brewer.pal(8, "Dark2")[n]
  ggplot(df, aes(reorder(names, value), value)) +   
    geom_bar(stat = "identity", fill = my_color) +
    coord_flip() +
    xlab("") +
    ylab("") +
    theme_minimal()
}

ggplot_ngram(crps, n = 1)
ggplot_ngram(crps, n = 2)
ggplot_ngram(crps, n = 3)
