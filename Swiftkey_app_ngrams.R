#  ----------------------------------------------------------------------------
#  COURSERA JHU - DATA SCIENCE CAPSTONE
#  App and prediction algorithm Week 5
#  File: Swiftkey_app_ngrams.R
#  (c) 2016 - Enrique Pérez Herrero
#  GNU GENERAL PUBLIC LICENSE Version 2, June 1991
#  21/Apr/2016
#  ----------------------------------------------------------------------------

# DESCRIPTION:  Calculate ngrams for prediction using samples from data set, and
# save them on shiny app directory

# 0. CLEAR ENVIRONMENT AND LOAD PACKAGES

rm(list = ls(all = TRUE))
gc(reset = TRUE)

library(quanteda)
library(stringr)

source("Swiftkey_Data_Access_Functions.R")

# 1. PARAMETERS

AVAILABLE_LANGUAGES
LANGUAGE <- AVAILABLE_LANGUAGES[1]   #english
SAMPLE_SIZE <- c(1000,  # Number of lines: blogs
                 10000,  # news
                  0   # twitter
                 )
CREATE_SAMPLES <- TRUE
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

# 5.  LOAD DICTIONARY:
# cracklib-small included in ubuntu 15.10 distribution

cracklib_dic <- load_dictionary()

in_dictionary <- function(my_word){
  answer <- cracklib_dic[cracklib_dic == my_word]
  ifelse(length(answer), answer, UNK)
}

# 6. ALL SAMPLED CORPUS N-GRAMS FREQUENCIES

get_tokens <- function(my_corpus){
  tkz <- tokenize(
    toLower(my_corpus),
    ngrams = 1,
    removePunct = TRUE,
    removeNumbers = TRUE,
    removeTwitter = TRUE,
    verbose = TRUE
  )
  # Pass dictionary
  cat("Parsing tokens with dictionary...\n")
  tkz[[1]] <- sapply(tkz[[1]], in_dictionary)
}

get_ngrams_freq <- function(my_tokens, ngram = 2) {
  tkz <- quanteda::ngrams(my_tokens, n = ngram, concatenator = "_")
  my_dfm <-  dfm(tkz, toLower = FALSE)
  df_freq <- sort(colSums(my_dfm), decreasing = TRUE)
  df_freq  <- data.frame(names(df_freq), df_freq)
  names(df_freq) <- c("words", "frequency")
  if (ngram > 1) {
    df_freq$sentence <-
      word(df_freq$words, -ngram, -2, sep = fixed("_"))
    df_freq$prediction <- word(df_freq$words, -1, sep = fixed("_"))
    # Remove empty sentences
    df_freq <- df_freq[!df_freq$sentence == "",]
    # Do not predict UNK!
    df_freq <- df_freq[!df_freq$prediction == UNK,]
    # Drop words
    df_freq$words <- NULL
    # Reorder columns
    df_freq  <- df_freq[c("sentence", "prediction", "frequency")]
  }
  # Rename rows
  rownames(df_freq) <- c(1:nrow(df_freq))
  # Remove NA's
  df_freq  <- df_freq[complete.cases(df_freq),]
  return(df_freq)
}


# 7. FIND TOKENS

system.time({
  tokens <- get_tokens(crps)
})

# 8. CALCULATE N-GRAMS FOR PREDICTION

unigrams <- get_ngrams_freq(tokens, ngram = 1)
bigrams <- get_ngrams_freq(tokens, ngram = 2)
trigrams <- get_ngrams_freq(tokens, ngram = 3)
tetragrams <- get_ngrams_freq(tokens, ngram = 4)

# View(unigrams)
# View(bigrams)
# View(trigrams)
# View(tetragrams)

# 9. SAVE TO APP DIRECTORY

if(!dir.exists(app_dir)) dir.create(app_dir)

saveRDS(unigrams, paste0(app_dir, "1.ngr"))
saveRDS(bigrams, paste0(app_dir, "2.ngr"))
saveRDS(trigrams, paste0(app_dir, "3.ngr"))
saveRDS(tetragrams, paste0(app_dir, "4.ngr"))

