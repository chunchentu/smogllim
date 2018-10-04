library(R.matlab)
library(xLLiM)
library(smogllim)
library(mclust)

data_source = "OJ"
if(data_source=="OJ"){
    data_name = "OJ_data.mat"
} else if(data_source=="Face"){
    data_name = "Face_data.mat"
}


## settings
K = 10
M = 5
Lw = 10
minSize = 5
dropTh = 0.3
cstr = list(Sigma="i")

cvID = 1
set.seed(cvID)

# load data
temp = readMat(data_name)
yapp = temp$Y
tapp = temp$t

D = dim(yapp)[1]
N = dim(yapp)[2]
Lt = dim(tapp)[1]

if(data_source=="OJ"){
    data_name = "OJ_data.mat"
    test_num = 20
} else if(data_source=="Face"){
    data_name = "Face_data.mat"
    test_num = 100
}
train_num = N - test_num

rand_index = sample(1:N)
test_index = sort(rand_index[1:test_num])
train_index = sort(rand_index[(test_num+1):N])

train_y = yapp[, train_index]
train_t = tapp[, train_index, drop=FALSE]
test_y = yapp[, test_index]
test_t = tapp[, test_index, drop=FALSE]


# get initialization
temp_cluster = Mclust(t(train_t), K, verbose=FALSE)
in_r = array(0, c(train_num, K))
for(i in 1:train_num){
    in_r[i, temp_cluster$classification[i]] = 1
}
gllim_result = gllim(train_t, train_y, K, in_r=NULL, Lw=Lw, cstr=cstr)
gllim_pred = gllim_inverse_map(yapp, gllim_result)
gllim_pred_t = gllim_pred$x_exp[1:Lt, , drop=FALSE]

gllim_train_diff = train_t - gllim_pred_t[, train_index, drop=FALSE]
gllim_train_mse = mean(apply(gllim_train_diff^2, 2, sum))
gllim_test_diff = test_t - gllim_pred_t[, test_index, drop=FALSE]
gllim_test_mse = mean(apply(gllim_test_diff^2, 2, sum))
cat(sprintf("GLLiM: Train MSE: %.4f, test MSE: %.4f\n",
                                            gllim_train_mse, gllim_test_mse))


gllim_r = round(gllim_result$r)

cluster_assign = apply(gllim_r, 1, which.max)
temp_r = array(0, c(N, 2))
for(c in sort(unique(cluster_assign))){
    index = which(cluster_assign==c)
    if(length(index)==1) {
        temp_r[index, ] = c(c, 1)
    } else {
        temp_t = train_t[, index, drop=FALSE]
        temp_cluster = Mclust(t(temp_t), M, verbose=FALSE)
        temp_assign = temp_cluster$classification
        temp_r[index, 1] = c
        temp_r[index, 2] = temp_assign
    }
}
in_r2 = array(0, c(train_num, K, M))
for(i in 1:train_num){
    in_r2[i, temp_r[i, 1], temp_r[i, 2]] = 1
}
smogllim_result = smogllim(train_t, train_y, K, M, in_r=in_r2, Lw=Lw, cstr=cstr,
                                                minSize=minSize, dropTh=dropTh)
smogllim_pred = smogllim_inverse_map(yapp, smogllim_result$theta)
smogllim_pred_t = smogllim_pred$x_exp[1:Lt, , drop=FALSE]

smogllim_train_diff = train_t - smogllim_pred_t[, train_index, drop=FALSE]
smogllim_train_mse = mean(apply(smogllim_train_diff^2, 2, sum))
smogllim_test_diff = test_t - smogllim_pred_t[, test_index, drop=FALSE]
smogllim_test_mse = mean(apply(smogllim_test_diff^2, 2, sum))
cat(sprintf("SMoGLLiM Train MSE: %.4f, test MSE: %.4f\n",
                                        smogllim_train_mse, smogllim_test_mse))
