library(R.matlab)
tmp = readMat("face_data.mat")
N = dim(tmp$Y)[2]

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

writeMat("face_cv_index.mat", cv_train_mat=cv_train_mat, cv_test_mat=cv_test_mat)
