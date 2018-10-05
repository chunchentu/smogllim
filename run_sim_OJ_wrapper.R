library(devtools)
load_all(".")
source("run_sim_fun.R")
args = (commandArgs(TRUE))
eval(parse(text=args[[1]])) # cvID
eval(parse(text=args[[2]])) # data_source 
eval(parse(text=args[[3]])) # save_prefix
# eval(parse(text=args[[2]])) # K
# eval(parse(text=args[[3]])) # M
# eval(parse(text=args[[4]])) # Lw
# eval(parse(text=args[[5]])) # minSize
# eval(parse(text=args[[6]])) # dropTh
# eval(parse(text=args[[7]])) # data_source 
# eval(parse(text=args[[8]])) # save_prefix
M = 5
minSize = 5
Lw = 8
for(K in c(5, 10, 15)){
    for(dropTh in seq(0.1, 1, 0.1)){
        cat(sprintf("cvID: %d, K: %d, M: %d, Lw: %d\n minSize: %d, dropTh: %.2f\n save_prefix: %s\n", cvID, K, M, Lw, minSize, dropTh, save_prefix))
        run_sim_fun(cvID, K, M, Lw, minSize, dropTh, data_source=data_source, save_prefix=save_prefix)
    }
}



