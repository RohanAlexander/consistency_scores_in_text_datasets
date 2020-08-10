library(tidyverse)
library(tau)
library(tm)
library(stringr)
library(stats)
library(reshape2)
library(RWeka)

#setwd("~/Desktop/repos/consistency_scores_in_text_datasets/shinyapp")
path <- getwd()
demoTraining <- readLines(paste(path,"/samples/demo.txt", sep=""), warn=FALSE)

tokenizer <- function(corpus) {
  lines <- vector()
  # Read the corpus line by line
  for (line in corpus) {
    # add two start-of-sentence tokens to the beginning of a sentence
    #line <- paste("<s> <s>", line)
    lines <-c(lines, line)
  }
  lines <- tolower(lines)
  lines <- gsub("[.!?]$|[.!?] |$", " </s>", lines) # add end-of-sentence token to the end of a sentence
  tokens <- unlist(strsplit(lines, "[^a-z<>/]"))
  tokens <- tokens[tokens != ""]
  return(tokens)
}

tokens <- tokenizer(demoTraining)

library(data.table)
unigrams <- textcnt( tokens, n=1, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)
bigrams  <- textcnt( tokens, n=2, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)
trigrams <- textcnt( tokens, n=3, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)

# Remove trigram that has "</s>" in it, so the trigrams are constructed within a sentence
unigrams <- unigrams[!grepl("</s>", names(unigrams))]
bigrams <-   bigrams[!grepl("</s>", names(bigrams))]
trigrams <- trigrams[!grepl("</s>", names(trigrams))]

### ngram probability
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
  sub(" [a-z']+$", "", string)
}

# Kneser-Ney Smoothing
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

# Get the probabilities after smoothing
unigramProbs <- kneserNay(unigrams, 0.75)
bigramProbs <- kneserNay(bigrams, 0.75)
trigramProbs <- kneserNay(trigrams, 0.75)

library(data.table)

unigramDF <- data.table("Words" = (names(unigrams)), 
                        "Probability" = as.vector(unigramProbs), stringsAsFactors=F)
bigramsDF <- data.table("First Words" = removeLastWord(names(bigrams)), 
                        "Las tWord" = getLastWords(names(bigrams), 1), 
                        "Probability" = as.vector(bigramProbs), stringsAsFactors=F)
trigramsDF <- data.table("Firs tWords" = removeLastWord(names(trigrams)), 
                         "Last Word" = getLastWords(names(trigrams), 1), 
                         "Probability" = as.vector(trigramProbs), stringsAsFactors=F)

library(dplyr)
unigramDF <- (unigramDF %>% arrange(desc(Probability)))
bigramsDF <- bigramsDF %>% arrange(desc(Probability)) %>% filter(Probability > 0.0001)
trigramsDF <- trigramsDF %>% arrange(desc(Probability)) %>% filter(Probability > 0.0001)

detector <- function(input) {
  inputCopy <- unlist(strsplit(input, "\n")) # split the input by sentences
  tokens <- tokenizer(inputCopy)
  # Turn input text to trigrams
  inputTrigrams <- textcnt( tokens, n=3, method = "string", split = "[[:space:][:digit:]]+", decreasing = TRUE)
  inputTrigrams <- inputTrigrams[!grepl("</s>", names(inputTrigrams))]
  print(inputTrigrams)
  correctSentence <- vector()
  
  for (trigram in names(inputTrigrams)) {
    firstWords <- removeLastWord(trigram)
    lastWord <- getLastWords(trigram, 1)
    if(!(lastWord %in% dplyr::filter(trigramsDF, firstWords == FirstWords)$LastWord)){
      correctWord <- dplyr::filter(trigramsDF, firstWords== FirstWords)$LastWord
      print(correctSequence)
    }# else {
      #return(input)
    #}
  }
  return(correctSequence)
}
detector("wo had arwg")

