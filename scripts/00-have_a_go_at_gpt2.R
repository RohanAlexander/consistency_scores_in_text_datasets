# Based on: https://blogs.rstudio.com/ai/posts/2019-10-23-gpt-2/



remotes::install_github("r-tensorflow/gpt2")

gpt2::install_gpt2(envname = "r-gpt2")

library(gpt2)

install.packages("reticulate")
library(reticulate)
use_virtualenv("r-tensorflow")
install.packages("keras")
library(keras)
import("scipy")

install_gpt2()

gpt2(model = "124M")

