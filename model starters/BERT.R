# Source tutorial: https://blogs.rstudio.com/ai/posts/2019-09-30-bert-r/
Sys.setenv(TF_KERAS=1) 
# to see python version
reticulate::py_config()
setwd("~/Desktop/repos/consistency_scores_in_text_datasets/model starters")

# 1. Read text from image
library(tesseract)
eng <- tesseract("eng")
text_original <- tesseract::ocr("BERT Python/image1.png", engine = eng)
cat(text_original)

# 2. Collect incorrect words
library(textclean)
text_stripped <- strip(text_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                       lower.case = FALSE)
library(hunspell)
incorrectwords <- hunspell(text_stripped)

# 3. Configure paths to the BERT pre-trained files variables
### The zip file is available through the tutorial link at the first line
pretrained_path = 'uncased_L-12_H-768_A-12'
config_path = file.path(pretrained_path, 'bert_config.json')
checkpoint_path = file.path(pretrained_path, 'bert_model.ckpt')
vocab_path = file.path(pretrained_path, 'vocab.txt')
seq_length = NULL

# 4. Load BERT and tokenize the text
library(reticulate)
k_bert = import('keras_bert')
token_dict = k_bert$load_vocabulary(vocab_path)
tokenizer = k_bert$Tokenizer(token_dict)
model = k_bert$load_trained_model_from_checkpoint(
  config_path,
  checkpoint_path,
  seq_len=seq_length,
  training=T)

# 5. Tokenize the text and get the encoding
tokens <- tokenizer$tokenize(text_original)
encodes <- tokenizer$encode(text_original)

# 6. Get encoded inputs for the model: train, segments, targets
indices = encodes[1]
segments = encodes[2]
## Manually flag word 13 and word 33 as masked words to be predicted 
masks = list(list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0))
train = do.call(cbind,indices) %>% t()
segments = do.call(cbind,segments) %>% t()
targets = do.call(cbind,masks) %>% t()


####### This part is generating error #########
# 7. Get the prediction matrix
prediction <- model$predict(c(train))

# 8. Use the index provided by prediction matrix to look for the predicted word from the token_dict
## This step is missing because it's blocked by step 7.
