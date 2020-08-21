library(tidyverse)
library(tau)
library(tm)
library(stringr)
library(stats)
library(reshape2)
library(RWeka)

#setwd("~/Desktop/repos/consistency_scores_in_text_datasets/shinyapp")
path <- getwd()
demoTraining <- readLines(paste(path,"/samples/sample_en_US.blogs.txt", sep=""), warn=FALSE)

######## OCR Read text from image ########
library(tesseract)
eng <- tesseract("eng")
text_original <- tesseract::ocr("BERT Python/image1.png", engine = eng)
cat(text_original)

######## Training Text Tokenization ######## 
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

######## Collect incorrect words ######## 
text_original <- "There was na possibility of taking a wlak that day"
library(textclean)
text_stripped <- strip(text_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                       lower.case = FALSE)
library(hunspell)
incorrectwords <- hunspell(text_stripped)
text_tokens <- tokenizer(text_stripped)
indices <- match(as.vector(incorrectwords[[1]]),text_tokens)
bigrams <- vector()
for(i in indices) {
  bigram <- paste(text_tokens[i-2], text_tokens[i-1])
  bigrams <- append(bigrams, bigram)
}

########  ngram probability ######## 
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
  sub(" [a-z']+$", "", string)
}

######## Kneser-Ney Smoothing ########
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
bigramsDF <- data.table("FirstWords" = removeLastWord(names(bigrams)), 
                        "LastWord" = getLastWords(names(bigrams), 1), 
                        "Probability" = as.vector(bigramProbs), stringsAsFactors=F)
trigramsDF <- data.table("FirstWords" = removeLastWord(names(trigrams)), 
                         "LastWord" = getLastWords(names(trigrams), 1), 
                         "Probability" = as.vector(trigramProbs), stringsAsFactors=F)

library(dplyr)
unigramDF <- (unigramDF %>% arrange(desc(Probability)))
bigramsDF <- bigramsDF %>% arrange(desc(Probability)) %>% filter(Probability > 0.0001)
trigramsDF <- trigramsDF %>% arrange(desc(Probability)) %>% filter(Probability > 0.0001)

######## Function to predict words ########
predict_words <- function(input) {
  prediction <- c()
  prediction <- c(prediction, filter(trigramsDF, getLastWords(input, 2) == FirstWords)$LastWord)
  return(prediction[1:5])
}

##### Try the prediction function #####
