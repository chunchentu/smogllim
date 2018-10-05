# this is used for testing behavior of different dropTH
source("run_sim_fun.R")
# args = (commandArgs(TRUE))
# eval(parse(text=args[[1]])) # cvID
# eval(parse(text=args[[2]])) # data_source
# eval(parse(text=args[[3]])) # save_prefix
cvID = 1
data_source = "OJ"
save_prefix = "."


M = 5
minSize = 5
Lw = 8
K = 5
for(dropTh in seq(0.1, 1, 0.1)){
    cat(sprintf("cvID: %d, K: %d, M: %d, Lw: %d\n minSize: %d, dropTh: %.2f\n save_prefix: %s\n", cvID, K, M, Lw, minSize, dropTh, save_prefix))
    run_sim_fun(cvID, K, M, Lw, minSize, dropTh, data_source=data_source, save_prefix=save_prefix)
}




