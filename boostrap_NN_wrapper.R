library(devtools)
load_all(".")
source("boostrap_NN_fun.R")
args = (commandArgs(TRUE))
eval(parse(text=args[[1]])) # cvID
