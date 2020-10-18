transformers <- import("transformers")
DistilBertForMaskedLM <- transformers$DistilBertForMaskedLM
DistilBertModel <- transformers$DistilBertModel
DistilBertTokenizer <- transformers$DistilBertTokenizer
tokenizer <- DistilBertTokenizer$from_pretrained('distilbert-base-uncased')

# pip install "transformers==3.1.0"
model <- DistilBertModel$from_pretrained('distilbert-base-uncased', return_dict=TRUE)
