# Load all dependencies
library(reticulate)
use_condaenv("my-torch", required = TRUE) # set Python env to conda
library(textclean)
library(hunspell)
library(stringr)

library(reticulate)
install_bert <- function() {
  reticulate::py_install("pytorch")
  reticulate::py_install("torchvision")
  reticulate::py_install("transformers")
}

install_bert()

library(rTorch)
transformer <- reticulate::import("transformers")

BertForMaskedLM <- transformer$BertForMaskedLM
BertTokenizer <- transformer$BertTokenizer
tokenizer <- BertTokenizer$from_pretrained('bert-base-uncased')
model <- BertForMaskedLM$from_pretrained('bert-base-uncased')



text_original <- c("His Majesty Government wish to add that they have no im@@@l@fon of requesting the establishment of military bases in peace time within the area of Palestine now united to the Kis@@®m of Jordan.")


##### Prediction function
predict_tokens <- function(text_original) {
  ### Clean text
  text_stripped <- strip(text_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                         lower.case = FALSE)
  ### Collect text errors
  errors <- hunspell(text_stripped)

  ### Replace errors to [MASK]
  for(e in errors){
    pats <- paste(e, collapse = '|')
  }
  text <- str_replace_all(text_stripped, pats, '[MASK]')

  ### Tokenize the text
  tokenized_text <- tokenizer$tokenize(text)
  indexed_tokens <- tokenizer$convert_tokens_to_ids(tokenized_text)

  ### Encode the text and create segments tensors
  MASKIDS <- which(tokenized_text == "[MASK]")
  SEGS <- which(tokenized_text == ".")

  segments_ids <- rep(as.integer(0), times = SEGS)

  segments_tensors <- torch$tensor(list(segments_ids))
  tokens_tensor <- torch$tensor(list(indexed_tokens))

  ### Generate prediction
  py <- torch$no_grad()
  with(py, {
    predictions <- model(tokens_tensor, segments_tensors)
  })

  predicted_token <- list()
  for(i in 1:length(MASKIDS)){
    preds <- torch$topk(predictions[[1]][1][MASKIDS[i]], k=as.integer(50))
    indices <- preds$indices$tolist()
    list <- tokenizer$convert_ids_to_tokens(indices)
    predicted_token <- append(predicted_token, list(list[1]))
  }
  predicted_token.i <- 1

  for (i in 1:length(text)) {
    while (grepl(pattern = "\\Q[MASK]\\E", text[i])) {
      text[i] <- sub(pattern = "\\Q[MASK]\\E", replacement = predicted_token[predicted_token.i], x = text[i])
      predicted_token.i <- predicted_token.i + 1
    }
  }

  return(text)
}

predict_tokens(text_original)

library(reticulate)
install_miniconda()
conda_install(envname = 'r-reticulate', c('torch', 'transformers==3.1.0'), pip = TRUE)
