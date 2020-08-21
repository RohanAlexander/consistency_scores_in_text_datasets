# Source tutorial: https://blogs.rstudio.com/ai/posts/2019-10-23-gpt-2/
remotes::install_github("r-tensorflow/gpt2")
gpt2::install_gpt2(envname = "r-gpt2")
library(gpt2)
gpt2(prompt = "GPT-3 thinks GPT-2 is", model = "124M", seed = NULL, batch_size = 1, total_tokens = NULL,
     temperature = 1, top_k = 0, top_p = 1)
