library(rdrop2)
drop_auth()
token <- drop_auth()
saveRDS(token, file = "token.rds")
drop_dir() %>% 
  filter(.tag == "Arianna")

trigramsDF <- drop_read_csv("Arianna/trigramsDF.csv")

runExample("09_upload")
