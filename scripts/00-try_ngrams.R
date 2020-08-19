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
example_paragraph <- "I should like to bury something precious in every place 
where I've been happy and then, when I'm old and ugly and miserable, I could 
come back and dig it up and remember. I should like to also ensure that my children
and grandchildren knew where they were. For when I am old and ugly, I should like 
that they too dig it up and imagine me as I am now - happy and content. I should lIke 
also to be to be happy and content then, but really, who can promise that?"

# Create tokens
tokens_from_example <- quanteda::tokens(example_paragraph, remove_punct = TRUE)

# Create ngrams from the tokens
toks_ngram <- quanteda::tokens_ngrams(tokens_from_example, n = 3)
head(toks_ngram[[1]])

# Convert to tibble so we can use our familiar verbs
all_tokens <- tibble(tokens = toks_ngram[[1]])

# We only want the common ones, not every one.
all_tokens <- 
  all_tokens %>% 
  group_by(tokens) %>% 
  count() %>% 
  filter(n > 1) %>% 
  ungroup()

# Create a tibble that has the first two words in one column then the third
all_tokens <- 
  all_tokens %>% 
  mutate(tokens = str_replace_all(tokens, "_", " "),
         first_words = word(tokens, start = 1, end = 2),
         last_word = word(tokens, -1),
         tokens = str_replace_all(tokens, " ", "_"),
         first_words = str_replace_all(first_words, " ", "_")
         ) %>% 
  rename(last_word_expected = last_word) %>% 
  select(-n, -tokens)


#### Apply to the dataset to create a consistency score ####
# Now that we have our collection of n-grams (this will be internal consistency
# because that collection was based on the data itself) we want to work out a measure
# of consistency.
example_sentence_with_errors <- "I should lIke also to be to be happy and content 
then, but really, who can promise that?"

# Create tokens with errors
tokens_from_example_with_errors <- quanteda::tokens(example_sentence_with_errors, remove_punct = TRUE)

# Create ngrams from the tokens with errors
toks_ngram_with_errors <- quanteda::tokens_ngrams(tokens_from_example_with_errors, n = 3)

all_tokens_with_errors <- tibble(tokens = toks_ngram_with_errors[[1]])

all_tokens_with_errors <- 
  all_tokens_with_errors %>% 
  mutate(tokens = str_replace_all(tokens, "_", " "),
         first_words = word(tokens, start = 1, end = 2),
         last_word = word(tokens, -1),
         tokens = str_replace_all(tokens, " ", "_"),
         first_words = str_replace_all(first_words, " ", "_")
  ) 

# Now we combine them so last_word will be what we have and last_word_expected will 
# be what we expect.
all_tokens_with_errors <- 
  all_tokens_with_errors %>% 
  left_join(all_tokens, by = c("first_words"))

# Calculate the internal consistency score:
all_tokens_with_errors %>% 
  mutate(as_expected = last_word == last_word_expected) %>% 
  count(as_expected) %>% 
  filter(!is.na(as_expected)) %>% 
  mutate(consistency = n / sum(n)) %>% 
  filter(as_expected == TRUE)

# Identify which words were unexpected
all_tokens_with_errors %>% 
  mutate(as_expected = last_word == last_word_expected) %>% 
  filter(as_expected == FALSE)
