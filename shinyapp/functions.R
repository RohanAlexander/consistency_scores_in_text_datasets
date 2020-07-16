library(tidyverse)
library(tau)
library(tm)
library(hash)

message("load data...")
message(Sys.time())
setwd(getSrcDirectory(function(){}))

# Load training text
blog.sample <- readLines("samples/*.txt")

# Divide the data to 50 batches to speed up n-gram processing later
nbatch <- 50
blog.len <- ceiling(length(blog.sample) / nbatch)

# Build functions to preprocess the text by removing non-English characters, twitter handles and urls
removeURL <- function(x) gsub("http\\S+", "", x)
removeHash <- function(x) gsub("[@#&]\\S+", "", x)
removeNumPunct <- function(x) gsub("[^A-z[:space:]']*", "", x)

# Build the n-gram
h <- hash()

# Loop over the batches
for (b in 1:nbatch -1) {
  message(sprintf("Processing the %i-th batch", b))
  
  #concatenate text and preprocess
  blog <- blog.sample[blog.len * b + (1 : blog.len)]
  trainingText <- blog %>%
    removeURL() %>% removeHash() %>% removeNumPunct() %>% tolower() %>% stripWhitespace()

  # Get trigrams
  trigram <- textcnt( trainingText, n=3, split=" ", method = "string", decreasing = TRUE)

  # Create hash table for fast prediction
  for (i in 1: length(trigram)) {
    if (trigram[i] < 4) {
      break
    } else {
      gram <- strsplit(names(trigram[i]), split = ' ')[[1]]
      history <- paste0(gram[1:2], collapse = ' ')
      candidate <- gram[3]
      count <- trigram[[i]]
      if (candidate %in% h[[history]]$candidate) {
        index <- h[[history]]$candidate == candidate
        h[[history]]$count[index] <- h[[history]]$count[index] + count
      } else {
        h[[history]]$candidate <- c(h[[history]]$candidate, candidate)
        h[[history]]$count <- c(h[[history]]$count, count)
      }
    }
  }
  gc()
}
if (1) {
  save(h, file = "hashtable.Rdata")
}

