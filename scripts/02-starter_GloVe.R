# Source tutorial: https://keras.rstudio.com/articles/examples/pretrained_word_embeddings.html
# This example shows how one can quickly load glove vectors
# and train a Keras model in R

library(keras)
library(dplyr)

# Download Glove vectors if necessary
if (!file.exists('glove.6B.zip')) {
  download.file('http://nlp.stanford.edu/data/glove.6B.zip',destfile = 'glove.6B.zip')
  unzip('glove.6B.zip')
}

# load glove vectors into R
vectors = data.table::fread('glove.6B.300d.txt', data.table = F,  encoding = 'UTF-8')
colnames(vectors) = c('word',paste('dim',1:300,sep = '_'))

# structure of the vectors
as_tibble(vectors)
