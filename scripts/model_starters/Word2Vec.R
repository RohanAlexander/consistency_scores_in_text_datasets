# Source tutorial: https://blogs.rstudio.com/ai/posts/2017-12-22-word-embeddings-with-keras/

library(readr)
library(stringr)

# Using a short text for demonstration
training_corpus <- "There was no possibility of taking a walk that day. We had been wandering, indeed, in the leafless shrubbery an hour in the morning. 
But since dinner (Mrs Reed, when there was no company, dined early). The cold winter wind had brought with it clouds so sombre, and a rain so penetrating, 
that further out-door exercise was now out of the question."

library(keras)
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(training_corpus)

library(reticulate)
library(purrr)
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}

embedding_size <- 128  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.

input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

summary(model)

#model <- keras::train_on_batch(model)

model %>%
  fit_generator(
    skipgrams_generator(training_corpus, tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 10, epochs = 1
  )
