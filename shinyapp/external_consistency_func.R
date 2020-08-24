#### Create bigrams from text errros' previous two words ####
library(textclean)
library(hunspell)
library(tidyverse)
library(rdrop2)
trigramsDF <- drop_read_csv("Arianna/trigramsDF.csv")
#trigramsDF <- read_csv("trigramsDF.csv")

# Tokenizer function
tokenizer <- function(corpus) {
  lines <- vector()
  # Read the corpus line by line
  for (line in corpus) {
    lines <-c(lines, line)
  }
  lines <- tolower(lines)
  lines <- gsub("[.!?]$|[.!?] |$", " </s>", lines) # add end-of-sentence token to the end of a sentence
  tokens <- unlist(strsplit(lines, "[^a-z<>/]"))
  tokens <- tokens[tokens != ""]
  return(tokens)
}

####  Ngram probability ####
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}
create_bigrams_for_predictions <- function(input_original) {
  
  input_stripped <- strip(input_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                          lower.case = FALSE)
  # Use hunspell spell checker to get text errors
  incorrectwords <- hunspell(input_stripped)
  incorrectwords <- lapply(incorrectwords, tolower)
  # Split the input text
  input_tokens <- tokenizer(input_stripped)
  # Get the indices of the incorrect words in the tokens
  incorrectwords_indices <- match(as.vector(incorrectwords[[1]]),input_tokens)
  # Get the previous two words of the text error and put them in the list of bigrams
  bigrams_for_prediction <- vector()
  for(i in incorrectwords_indices) {
    previous_two_words <- paste(input_tokens[i-2], input_tokens[i-1])
    bigrams_for_prediction <- append(bigrams_for_prediction, previous_two_words)
  }
  split_trigrams <- data.frame(bigrams_for_prediction, "incorrectwords" = incorrectwords[[1]])
  
  return(split_trigrams)
}

#### Function to predict words ####
predict_words <- function(split_trigrams) {
  expected_words <- c()
  for (i in 1: nrow(split_trigrams)) {
    expected_word <- filter(trigramsDF, getLastWords(split_trigrams["bigrams_for_prediction"][i,], 2) == FirstWords)$LastWord
    expected_word <- expected_word[1]
    expected_words <- c(expected_words, expected_word)
  }
  results <- data.frame(split_trigrams["bigrams_for_prediction"], split_trigrams["incorrectwords"], expected_words)
  return(results)
}

