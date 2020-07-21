library(tidyverse)
library(tau)
library(tm)
library(stringr)
library(stats)

#setwd("~/Desktop/repos/consistency_scores_in_text_datasets/shinyapp")

demoTraining <- "There was no possibility of taking a walk that day. 
We had been wandering, indeed, in the leafless shrubbery an hour in the morning; 
but since dinner (Mrs. Reed, when there was no company, dined early) 
the cold winter wind had brought with it clouds so sombre, and a rain so penetrating, 
that further out-door exercise was now out of the question."

tokenizer <- function(lines) {
  lines <- tolower(lines)
  lines <- gsub("'", "'", lines)
  #lines <- gsub("[.!?]$|[.!?] |$", " ''split'' ", lines)
  tokens <- unlist(strsplit(lines, "[^a-z']"))
  tokens <- tokens[tokens != ""]
  return(tokens)
}
demoTraining <- tokenizer(demoTraining)
unigrams <- textcnt( demoTraining, n=1, method = "string", decreasing = TRUE)
bigrams  <- textcnt( demoTraining, n=2, method = "string", decreasing = TRUE)
trigrams <- textcnt( demoTraining, n=3, method = "string", decreasing = TRUE)

userInput <- "There was no possibility of taking a walk that day. 
We had been wandering, indeed, in the leafless shrubbery an hour in the morning; 
but since dinner (Mrs. Reed, when there was no company, dined early) 
the cold winter wind had brought with it clouds so sombre, and a rain so penetrating, 
that further out-door exercise was now out of the question."
userInput <- tokenizer(userInput)
ui_unigrams <- textcnt( userInput, n=1, method = "string", decreasing = TRUE)
ui_bigrams  <- textcnt( userInput, n=2, method = "string", decreasing = TRUE)
ui_trigrams <- textcnt( userInput, n=3, method = "string", decreasing = TRUE)
userInputNgram <- ui_trigrams

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

unigramProbs <- kneserNay(unigrams, 0.75)
bigramProbs <- kneserNay(bigrams, 0.75)
trigramProbs <- kneserNay(trigrams, 0.75)

createModel <- function(n, threshold,userInputNgram ) {
  ngrams <- list(bigramProbs, trigramProbs)[[n-1]]
  model <- ngrams[getLastWords(names(userInputNgram), n-1)]
  names(model) <- names(userInputNgram)
  if(n > 3) model[is.na(model) | model < threshold] <- 
    trigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 3)]
  if(n > 2) model[is.na(model) | model < threshold] <- 
    bigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 2)]
  if(n > 1) model[is.na(model) | model < threshold] <- 
    unigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 1)]
  return(model)
}

library(data.table)

unigramDF <- data.table("Words" = (names(unigrams)), "Probability" = unigramProbs, stringsAsFactors=F)

bigramsDF <- data.table("FirstWords" = removeLastWord(names(bigrams)), 
                        "LastWord" = getLastWords(names(bigrams), 1), 
                        "Probability" = bigramProbs, stringsAsFactors=F)

trigramsDF <- data.table("FirstWords" = removeLastWord(names(trigrams)), 
                         "LastWord" = getLastWords(names(trigrams), 1), 
                         "Probability" = trigramProbs, stringsAsFactors=F)


library(dplyr)
# unigramDF <- (unigramDF %>% arrange(desc(Probability)))
bigramsDF <- bigramsDF %>% arrange(desc(Probability)) %>% dplyr::filter(Probability > 0.0001)
trigramsDF <- trigramsDF %>% arrange(desc(Probability)) %>% dplyr::filter(Probability > 0.0001)

detector <- function(input) {
  input <- input
  inputTrigrams <- textcnt( input, n=3, method = "string", decreasing = TRUE)
  for (trigram in names(inputTrigrams)) {
    firstWords <- removeLastWord(trigram)
    lastWord <- getLastWords(trigram, 1)
    if(!(lastWord %in% dplyr::filter(trigramsDF, firstWords == FirstWords)$LastWord)){
      print(lastWord)
      correct <- paste(dplyr::filter(trigramsDF, firstWords == FirstWords)$FirstWords,dplyr::filter(trigramsDF, firstWords== FirstWords)$LastWord)
      return(correct)
    } else {
      return(input)
    }
  }
  return(input)
}

predictor <- function(input) {
  n <- length(strsplit(input, " ")[[1]])
  prediction <- c()
  if(n >= 2 && length(prediction)<3) 
    prediction <- c(prediction, stats::filter(trigramsDF, getLastWords(input, 2) == FirstWords)$LastWord)
  if(n >= 1 && length(prediction)<3) 
    prediction <- c(prediction, stats::filter(bigramsDF, getLastWords(input, 1) == FirstWords)$LastWord)
  # if(length(prediction)<3 ) prediction <- c(prediction, unigramDF$Words)
  return(unique(prediction)[1:3])
}

