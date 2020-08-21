library(tidyverse)
library(tau)
library(tm)
library(stringr)
library(stats)
library(reshape2)
library(RWeka)

#setwd("~/Desktop/repos/consistency_scores_in_text_datasets/scripts/")
path <- getwd()
demoTraining <- readLines(paste(path,"/input-sample_en_US.blogs.txt", sep=""), warn=FALSE)

#### OCR Read text from image ####
# To-do: make an UI for this to upload PDF
library(tesseract)
eng <- tesseract("eng")
input_original <- tesseract::ocr("input-image.png", engine = eng)
cat(input_original)

#### Collect incorrect words from the OCR ####
library(textclean)
input_stripped <- strip(input_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                       lower.case = FALSE)
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

library(hunspell)
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

#### Training text tokenization ####
tokens <- tokenizer(demoTraining)

#### Make data table for the ngrams ####
library(data.table)
unigrams <- textcnt( tokens, n=1, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)
bigrams  <- textcnt( tokens, n=2, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)
trigrams <- textcnt( tokens, n=3, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)

# Remove trigram that has "</s>" in it, so the trigrams are constructed within a sentence
unigrams <- unigrams[!grepl("</s>", names(unigrams))]
bigrams <-   bigrams[!grepl("</s>", names(bigrams))]
trigrams <- trigrams[!grepl("</s>", names(trigrams))]

####  Ngram probability ####
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
  sub(" [a-z']+$", "", string)
}

#### Kneser-Ney smoothing ####
# To-do: Understand and rewrite the function
kneserNay <- function(ngrams, d) {
  n <- length(strsplit(names(ngrams[1]), " ")[[1]])
  # Special case for unigrams
  if(n==1) {
    noFirst <- unigrams[getLastWords(names(bigrams), 1)]
    pContinuation <- table(names(noFirst))[names(unigrams)] / length(bigrams)
    return(pContinuation)
  }
  
  # Get needed counts
  nMinusOne <- list(unigrams, bigrams, trigrams)[[n-1]]
  noLast <- nMinusOne[removeLastWord(names(ngrams))]
  noFirst <- nMinusOne[getLastWords(names(ngrams), n-1)]
  
  # Calculate discounts, lambda and pContinuation
  discounts <- ngrams - d
  discounts[discounts < 0] <- 0
  lambda <- d * table(names(noLast))[names(noLast)] / noLast
  if(n == 2) pContinuation <- table(names(noFirst))[names(noFirst)] / length(ngrams)
  else pContinuation <- kneserNay(noFirst, d)
  
  # Put it all together
  probabilities <- discounts / noLast + lambda * pContinuation / length(ngrams)
  return(probabilities)
}

#### Get the probabilities after smoothing ####
unigramProbs <- kneserNay(unigrams, 0.75)
bigramProbs <- kneserNay(bigrams, 0.75)
trigramProbs <- kneserNay(trigrams, 0.75)

# Make a data table for the ngrams
library(data.table)
unigramDF <- data.table("Words" = (names(unigrams)), 
                        "Probability" = as.vector(unigramProbs), stringsAsFactors=F)
bigramsDF <- data.table("FirstWords" = removeLastWord(names(bigrams)), 
                        "LastWord" = getLastWords(names(bigrams), 1), 
                        "Probability" = as.vector(bigramProbs), stringsAsFactors=F)
trigramsDF <- data.table("FirstWords" = removeLastWord(names(trigrams)), 
                         "LastWord" = getLastWords(names(trigrams), 1), 
                         "Probability" = as.vector(trigramProbs), stringsAsFactors=F)

library(dplyr)
unigramDF <- unigramDF %>% arrange(desc(Probability))
bigramsDF <- bigramsDF %>% arrange(desc(Probability)) %>% filter(Probability > 0.0001)
trigramsDF<- trigramsDF %>% arrange(desc(Probability)) %>% filter(Probability > 0.0001)

#### Function to predict words ####
predict_words <- function(bigrams_for_prediction) {
  expected_words <- c()
  for (i in length(bigrams_for_prediction)) {
    expected_words <- c(expected_words, filter(trigramsDF, getLastWords(bigrams_for_prediction[i], 2) == FirstWords)$LastWord)
    expected_words <- expected_words[1:2] # Get the top 3 words
  }
  results <- data.frame(bigrams_for_prediction, incorrectwords = incorrectwords[[1]], expected_words)
  
  return(results)
}

##### Try the prediction function #####
# Take the previous two words of the text errors and predict the third word
predict_words(bigrams_for_prediction)
