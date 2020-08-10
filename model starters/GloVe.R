# Source tutorial: https://keras.rstudio.com/articles/examples/pretrained_word_embeddings.html
library(keras)

GLOVE_DIR <- 'glove.6B'
TEXT_DATA_DIR <- '20_newsgroup'
MAX_SEQUENCE_LENGTH <- 1000
MAX_NUM_WORDS <- 20000
EMBEDDING_DIM <- 100
VALIDATION_SPLIT <- 0.2

# download data if necessary
download_data <- function(data_dir, url_path, data_file) {
  if (!dir.exists(data_dir)) {
    download.file(paste0(url_path, data_file), data_file, mode = "wb")
    if (tools::file_ext(data_file) == "zip")
      unzip(data_file, exdir = tools::file_path_sans_ext(data_file))
    else
      untar(data_file)
    unlink(data_file)
  }
}
download_data(GLOVE_DIR, 'http://nlp.stanford.edu/data/', 'glove.6B.zip')

# first, build index mapping words in the embeddings set
# to their embedding vector

cat('Indexing word vectors.\n')

embeddings_index <- new.env(parent = emptyenv())
lines <- readLines(file.path(GLOVE_DIR, 'glove.6B.100d.txt'))
for (line in lines) {
  values <- strsplit(line, ' ', fixed = TRUE)[[1]]
  word <- values[[1]]
  coefs <- as.numeric(values[-1])
  embeddings_index[[word]] <- coefs
}

cat(sprintf('Found %s word vectors.\n', length(embeddings_index)))
