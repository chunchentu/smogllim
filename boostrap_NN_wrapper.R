library(devtools)
load_all(".")
source("boostrap_NN_fun.R")
args = (commandArgs(TRUE))
eval(parse(text=args[[1]])) # cvID
eval(parse(text=args[[2]])) # save_prefix
boostrap_NN_fun(cvID, save_prefix)
