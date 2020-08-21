# Source tutorial: https://blogs.rstudio.com/ai/posts/2019-09-30-bert-r/
Sys.setenv(TF_KERAS=1) 
# to see python version
reticulate::py_config()
setwd("~/Desktop/repos/consistency_scores_in_text_datasets/model starters")

# Read text from image
library(tesseract)
eng <- tesseract("eng")
text_original <- tesseract::ocr("BERT Python/image1.png", engine = eng)
cat(text_original)

# Collect incorrect words
library(textclean)
text_stripped <- strip(text_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
      lower.case = FALSE)
library(hunspell)
incorrectwords <- hunspell(text_stripped)

# Configure paths to the pre-trained files variables
pretrained_path = 'uncased_L-12_H-768_A-12'
config_path = file.path(pretrained_path, 'bert_config.json')
checkpoint_path = file.path(pretrained_path, 'bert_model.ckpt')
vocab_path = file.path(pretrained_path, 'vocab.txt')
seq_length = NULL

# Load BERT and tokenize the text
library(reticulate)
k_bert = import('keras_bert')
token_dict = k_bert$load_vocabulary(vocab_path)
tokenizer = k_bert$Tokenizer(token_dict)
encodes <- tokenizer$encode(text_original)
model = k_bert$load_trained_model_from_checkpoint(
  config_path,
  checkpoint_path,
  seq_len=seq_length,
  training=T)

tokens <- tokenizer$tokenize(text_original)
length(tokens)

# Get encoded inputs for the model: train, segments, targets
indices = encodes[1]
segments = encodes[2]
masks = list(list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0))
train = do.call(cbind,indices) %>% t()
segments = do.call(cbind,segments) %>% t()
targets = do.call(cbind,masks) %>% t()

# Get the prediction matrix
prediction <- model$predict(c(train))

