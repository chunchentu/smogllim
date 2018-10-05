library(R.matlab)
save_name = "OJ_cv_index.mat"
tmp = readMat("OJ_data.mat")
N = dim(tmp$Y)[2]
outlier_index = c(23, 27, 39, 42, 70, 78,82, 130, 133, 143, 147)
test_num = 20
test_candidate = setdiff(1:N, outlier_index)

cvNum = 50

cv_train_mat = array(0, c(N, cvNum))
cv_test_mat = array(0, c(N, cvNum))

for(cvID in 1:cvNum) {
    set.seed(cvID)
    test_index = sample(test_candidate, test_num)
    train_index = setdiff(1:N, test_index)
    cv_train_mat[train_index, cvID] = 1
    cv_test_mat[test_index, cvID] = 1
}

writeMat("OJ_cv_index.mat", cv_train_mat=cv_train_mat, cv_test_mat=cv_test_mat)
