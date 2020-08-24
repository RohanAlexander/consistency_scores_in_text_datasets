#### OCR Read text from image ####
library(tesseract)
eng <- tesseract("eng")
get_OCR_text<- function(file_path){
  input_original <- tesseract::ocr(file_path, engine = eng)
  return(input_original)
}
