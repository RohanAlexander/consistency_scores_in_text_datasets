#### Bug documentation: get_internal_consistency_score function in aRianna ####
# get_internal_consistency_score in this script does not
# return consistency score accurately. For example:

## text_to_check <- c("apple is red")
## internal_corpus <- c("apple is red, apple is green, apple is yellow,apple is red, apple is green, apple is yellow,")
## generate_internal_consistency_score(text_to_check, internal_corpus)

# The output will give an internal consistency socre of 0.33, instead of 1.
# This is because although `apple_is + red` is in the dataset, `red` is not in `apple_is + green` or `apple_is + yellow`
# The error is generated form the code chunk between line 52 to line 58

generate_internal_consistency_score <- function(text_to_check, consistency_dataset) {
  #### Apply to the dataset to create a consistency score ####
  # Now that we have our collection of n-grams (this will be internal consistency
  # because that collection was based on the data itself) we want to work out a measure
  # of consistency.

  # Create tokens with errors
  tokens_from_example_with_errors <- quanteda::tokens(text_to_check, remove_punct = TRUE)
  tokens_from_example_with_errors <- quanteda::tokens_tolower(tokens_from_example_with_errors)

  # Create ngrams from the tokens with errors
  toks_ngram_with_errors <- quanteda::tokens_ngrams(tokens_from_example_with_errors, n = 3)

  all_tokens_with_errors <- tibble::tibble(tokens = toks_ngram_with_errors[[1]])

  all_tokens_with_errors <-
    all_tokens_with_errors %>%
    dplyr::mutate(tokens = stringr::str_replace_all(tokens, "_", " "),
                  first_words = stringr::word(tokens, start = 1, end = 2),
                  last_word = stringr::word(tokens, -1),
                  tokens = stringr::str_replace_all(tokens, " ", "_"),
                  first_words = stringr::str_replace_all(first_words, " ", "_")
    )

  # Now we combine them so last_word will be what we have and last_word_expected will
  # be what we expect.
  all_tokens_with_errors <-
    all_tokens_with_errors %>%
    dplyr::left_join(dplyr::select(consistency_dataset, -tokens), by = c("first_words"))

  all_tokens_with_errors_only <- all_tokens_with_errors

  for(token in unique(all_tokens_with_errors_only$tokens)) {
    if(token %in% consistency_dataset$tokens){
      all_tokens_with_errors_only <- all_tokens_with_errors_only[all_tokens_with_errors_only$tokens != token, ]
    }
  }

  # Calculate the internal consistency score:
  internal_consistency <-
    all_tokens_with_errors %>%
    dplyr::mutate(as_expected = last_word == last_word_expected) %>%
    dplyr::count(as_expected) %>%
    dplyr::filter(!is.na(as_expected)) %>%
    dplyr::mutate(consistency = n / sum(n)) %>%
    dplyr::filter(as_expected == TRUE)

  # Identify which words were unexpected
  unexpected <-
    all_tokens_with_errors_only %>%
    dplyr::mutate(as_expected = last_word == last_word_expected) %>%
    dplyr::filter(as_expected == FALSE) %>%
    dplyr::select(-tokens)

  newList <- list("internal consistency" = internal_consistency,
                  "unexpected words" = unexpected)

  return(newList)
}
