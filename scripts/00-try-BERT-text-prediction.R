# Source tutorial: https://blogs.rstudio.com/ai/posts/2019-09-30-bert-r/
Sys.setenv(TF_KERAS=1)
# to see python version
reticulate::py_config()
setwd("~/Desktop/repos/consistency_scores_in_text_datasets/scripts/model_starters")

#### 1. Read text from image
library(tesseract)
eng <- tesseract("eng")
text_original <- tesseract::ocr("BERT_Python/image1.png", engine = eng)
cat(text_original)

#### 2. Collect incorrect words
library(textclean)
text_stripped <- strip(text_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                       lower.case = FALSE)

library(hunspell)
incorrectwords <- hunspell(text_stripped)

tokens <- unlist(strsplit(text_stripped, "[^aA-zZ<>/]"))
mask_indices <- match(incorrectwords[[1]],tokens)
for(i in mask_indices){
  tokens[i] = "MASK"
}
str <- toString(tokens)
str <- strip(str)
#### 3. Configure paths to the BERT pre-trained files variables
# The zip file is available through the tutorial link at the first line
pretrained_path = '../../../uncased_L-12_H-768_A-12'
config_path = file.path(pretrained_path, 'bert_config.json')
checkpoint_path = file.path(pretrained_path, 'bert_model.ckpt')
vocab_path = file.path(pretrained_path, 'vocab.txt')
seq_length = NULL

#### 4. Load BERT and tokenize the text
library(reticulate)
k_bert = import('keras_bert')
np = import('numpy')
token_dict = k_bert$load_vocabulary(vocab_path)
tokenizer = k_bert$Tokenizer(token_dict)
model = k_bert$load_trained_model_from_checkpoint(
  config_path,
  checkpoint_path,
  seq_len=seq_length,
  training=T)

library(zeallot)
#### 5. Tokenize the text and get the encoding
tokens <- tokenizer$tokenize(str)
encodes <- tokenizer$encode(str)

for(i in mask_indices){
  encodes[[1]][i+1] = 103
}

#### 6. Get encoded inputs for the model: train, segments, targets
indices <- encodes[1]
indices <- as.matrix(indices)
segments <- encodes[2]
segments <- as.matrix(segments)

## Manually flag word 13 and word 33 as masked words to be predicted
masks <- as.matrix(list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
train <- indices
targets <- masks

library(dplyr)
train = do.call(cbind,indices) %>% t()
segments = do.call(cbind,segments) %>% t()
targets = do.call(cbind,masks) %>% t()

#### 7. Get the prediction matrix
prediction <- model$predict(list(train,segments,targets))
predictions <- np$argmax(prediction[1], axis=-1L)

#### 8. Use the index provided by prediction matrix to look for the predicted word from the token_dict
token_dict[predictions[13]+1]
token_dict[predictions[33]+1]
