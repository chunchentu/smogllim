library(R.matlab)
library(smogllim)
library(mclust)
library(ggplot2)
library(tidyr)

set.seed(1234)
cstr = list(Sigma="i")
K = 10

data_name = "OJ.mat"
cv_name = "../OJ_cv_index.mat"

cvID = 1
temp = readMat(data_name)
yapp = t(temp$OJx)
tapp = (t(temp$OJy) - mean(temp$OJy))/sd(temp$OJy)

D = dim(yapp)[1]
N = dim(yapp)[2]
Lt = dim(tapp)[1]

# read in cv index
temp_cv = readMat(cv_name)
train_index = which(temp_cv$cv.train.mat[, cvID]==1)
test_index = which(temp_cv$cv.test.mat[, cvID]==1)

train_num = length(train_index)
test_num = length(test_index)

train_y = yapp[, train_index]
train_t = tapp[, train_index, drop=FALSE]
test_y = yapp[, test_index]
test_t = tapp[, test_index, drop=FALSE]

gllim_result = gllim(train_t, train_y, K, in_r=NULL, cstr=cstr)
gllim_r = round(gllim_result$r)

N = dim(train_y)[2]

cluster_assign = apply(gllim_r, 1, which.max)
temp_r = array(0, c(N, 2))
for(c in sort(unique(cluster_assign))){
index = which(cluster_assign==c)
if(length(index)<=M) {
    temp_r[index, 1] = c
    temp_r[index, 2] = 1
} else {
    temp_t = train_t[, index, drop=FALSE]
    temp_cluster = Mclust(t(temp_t), 1:M, verbose=FALSE)
    if(is.null(temp_cluster)){
        temp_r[index, 1] = c
        temp_r[index, 2] = 1
    } else {
        temp_assign = temp_cluster$classification
        temp_r[index, 1] = c
        temp_r[index, 2] = temp_assign
    }
}
}

r = array(0, c(N, K, M))
for(i in 1:N){
    r[i, temp_r[i, 1], temp_r[i, 2]] = 1
}

apply(r, 2:3, sum)

plot_t_func = function(target_global_cluster, target_local_cluster_1=1, target_local_cluster_2=2){

    idx1 = which(temp_r[, 1]==target_global_cluster & temp_r[, 2]==target_local_cluster_1)
    idx2 = which(temp_r[, 1]==target_global_cluster & temp_r[, 2]==target_local_cluster_2)

    d1 = data.frame(t=train_t[idx1], type="1", stringsAsFactors=TRUE)
    d2 = data.frame(t=train_t[idx2], type="2", stringsAsFactors=TRUE)

    d = rbind(d1, d2)
    d$idx = 1:nrow(d)

    ggplot(d) + geom_point(aes(x=idx, y=t), color=d$type)
    return(list(idx1=idx1, idx2=idx2))
}
idx = plot_t_func(8, 1, 2)
idx1 = train_index[idx$idx1]
idx2 = train_index[idx$idx2]

temp_d = data.frame(train_y[, idx1])

