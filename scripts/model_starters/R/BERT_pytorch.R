library(reticulate)
use_condaenv("my-torch", required = TRUE)

pytorch_pretrained_bert <- import("pytorch_pretrained_bert")
BertModel <- pytorch_pretrained_bert$BertModel
BertForMaskedLM <- pytorch_pretrained_bert$BertForMaskedLM
BertTokenizer <- pytorch_pretrained_bert$BertTokenizer
tokenizer <- BertTokenizer$from_pretrained('bert-base-uncased')

# pip install "transformers==3.1.0"
model <- BertForMaskedLM$from_pretrained('bert-base-uncased')

#### 1. Get text
text_original <- c("His Majesty Government wish to add that they have no im@@@l@fon of requesting the establishment of military bases in peace time within the area of Palestine now united to the Kis@@®m of Jordan.")

#### 2. Clean text
library(textclean)
text_stripped <- strip(text_original, char.keep = c("?", ".", "’", "~~"), digit.remove = TRUE, apostrophe.remove = FALSE,
                      lower.case = FALSE)

#### 3. Collect text errors
library(hunspell)
errors <- hunspell(text_stripped)

### 4. Replace errors to [MASK]
for(e in errors){
  pats <- paste(e, collapse = '|')
}
library(stringr)
text <- str_replace_all(text_stripped, pats, '[MASK]')

### 5. Tokenize the text
tokenized_text <- tokenizer$tokenize(text)
indexed_tokens <- tokenizer$convert_tokens_to_ids(tokenized_text)

### 6. Encode the text and create segments tensors
MASKIDS <- which(tokenized_text == "[MASK]")
SEGS <- which(tokenized_text == ".")

segments_ids <- rep(as.integer(0), times = SEGS)

library(rTorch)
segments_tensors <- torch$tensor(list(segments_ids))
tokens_tensor <- torch$tensor(list(indexed_tokens))

### 7. Generate prediction
py <- torch$no_grad()
with(py, {
  predictions <- model(tokens_tensor, segments_tensors)
})

### 8. Prediction function
predict_tokens <- function(text, predictions, MASKIDS) {
  predicted_token <- list()
  for(i in 1:length(MASKIDS)){
    preds <- torch$topk(predictions[[0]][MASKIDS[i]], k=as.integer(50))
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


predict_tokens(text, predictions, MASKIDS)

