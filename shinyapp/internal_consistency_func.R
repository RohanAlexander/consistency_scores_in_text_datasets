#### Preamble ####
# Contact: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Date: 19 August 2020
# Purpose: This script attempts to use n-grams to identify words that may be incorrect and 
# need to be corrected.

#### Workspace setup ####
library(quanteda)
library(tidyverse)

#### Generate comparison dataset ####
# First we need a collection of n-grams that we will search for, so we are creating
# that in this section. We will just look at 3-grams, so once we have this list 
# we can search for the first two tokens and then compare the third with what we
# expect.

create_ngrams_tibble <- function(corpus, is_training) {
  # Create tokens
  tokens <- quanteda::tokens(corpus, remove_punct = TRUE)
  # Create ngrams from the tokens
  toks_ngram <- quanteda::tokens_ngrams(tokens, n = 3)
  # Convert to tibble so we can use our familiar verbs
  all_tokens <- tibble(tokens = toks_ngram[[1]])
  
  # If it's a training corpus, we only want the common ones, not every one.
  if (is_training) {
    all_tokens <- 
      all_tokens %>% 
      group_by(tokens) %>% 
      count() %>% 
      filter(n > 1) %>% 
      ungroup()
  }
  
  # Create a tibble that has the first two words in one column then the third
  all_tokens <- 
    all_tokens %>% 
    mutate(tokens = str_replace_all(tokens, "_", " "),
           first_words = word(tokens, start = 1, end = 2),
           last_word = word(tokens, -1),
           tokens = str_replace_all(tokens, " ", "_"),
           first_words = str_replace_all(first_words, " ", "_")
    ) 
  
  # If it's a training corpus, drop n column and tokens column
  if (is_training) {
    all_tokens <- all_tokens %>% 
      rename(last_word_expected = last_word) %>% 
      select(-n, -tokens)
  }
  
  return(all_tokens)
}

# Get training n-gram tibble
#training_tokens<- create_ngrams_tibble(example_paragraph, is_training = TRUE)
# Get example n-gram tibble
#input_tokens<- create_ngrams_tibble(example_sentence_with_errors, is_training = FALSE)

#### Apply to the dataset to create a consistency score ####
# Now that we have our collection of n-grams (this will be internal consistency
# because that collection was based on the data itself) we want to work out a measure
# of consistency.
# Now we combine them so last_word will be what we have and last_word_expected will 
# be what we expect.

get_results <- function(training_tokens, input_tokens) {
  input_tokens_with_expected_word <- 
    input_tokens %>% 
    left_join(training_tokens, by = c("first_words"))
  
  # Calculate the internal consistency score:
  internal_consistency_score <- input_tokens_with_expected_word %>% 
    mutate(as_expected = last_word == last_word_expected) %>% 
    count(as_expected) %>% 
    filter(!is.na(as_expected)) %>% 
    mutate(consistency = n / sum(n)) %>% 
    filter(as_expected == TRUE)
  
  # Identify which words were unexpected and make them a table
  unexpected_words <- input_tokens_with_expected_word %>% 
    mutate(as_expected = last_word == last_word_expected) %>% 
    filter(as_expected == FALSE)
    results <- list(internal_consistency_score, unexpected_words)
  return(results)
}
