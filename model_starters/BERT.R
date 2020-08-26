# Source tutorial: https://blogs.rstudio.com/ai/posts/2019-09-30-bert-r/
Sys.setenv(TF_KERAS=1) 
# to see python version
reticulate::py_config()
setwd("~/Desktop/repos/consistency_scores_in_text_datasets/model_starters")


# 1. Configure paths to the BERT pre-trained files variables
### The zip file is available through the tutorial link at the first line
pretrained_path = '../../../uncased_L-12_H-768_A-12'
config_path = file.path(pretrained_path, 'bert_config.json')
checkpoint_path = file.path(pretrained_path, 'bert_model.ckpt')
vocab_path = file.path(pretrained_path, 'vocab.txt')
seq_length = NULL

# 2. Load BERT and tokenize the text
library(reticulate)
k_bert = import('keras_bert')
token_dict = k_bert$load_vocabulary(vocab_path)
tokenizer = k_bert$Tokenizer(token_dict)
model = k_bert$load_trained_model_from_checkpoint(
  config_path,
  checkpoint_path,
  seq_len=seq_length,
  training=T)
