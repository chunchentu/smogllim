library(R.matlab)
library(smogllim)
library(mclust)

run_sim_fun = function(cvID, K, M, Lw, minSize, dropTh,
                      data_source="OJ", save_prefix="."){

    ## settings
    # cvID = 1
    # K = 10
    # M = 5
    # Lw = 10
    # minSize = 5
    # dropTh = 0.3
    # data_source = "OJ"
    # save_prefix = "."


    dir.create(file.path(save_prefix, data_source), showWarnings = FALSE)
    save_prefix = file.path(save_prefix, data_source)
    save_name = file.path(save_prefix,
                          sprintf("smogllim_cv%d_K%d_M%d_Lw%d_size%d_th%.2f.Rdata",
                                  cvID, K, M, Lw, minSize, dropTh))
    cstr = list(Sigma="i")

    set.seed(cvID)

    # load data

    if(data_source=="OJ"){
        data_name = "OJ_data.mat"
        cv_name = "OJ_cv_index.mat"
    } else if(data_source=="face"){
        data_name = "face_data.mat"
        cv_name = "face_cv_index.mat"
    }
    temp = readMat(data_name)
    yapp = temp$Y
    tapp = temp$t

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

    gllim_result = gllim(train_t, train_y, K, in_r=NULL, Lw=Lw, cstr=cstr)
    gllim_pred = gllim_inverse_map(yapp, gllim_result)
    gllim_pred_t = gllim_pred$x_exp[1:Lt, , drop=FALSE]

    gllim_train_diff = train_t - gllim_pred_t[, train_index, drop=FALSE]
    gllim_train_mse = mean(apply(gllim_train_diff^2, 2, sum))
    gllim_test_diff = test_t - gllim_pred_t[, test_index, drop=FALSE]
    gllim_test_mse = mean(apply(gllim_test_diff^2, 2, sum))
    cat(sprintf("GLLiM: Train MSE: %.4f, test MSE: %.4f\n",
                                                gllim_train_mse, gllim_test_mse))

    smogllim_result = smogllim(train_t, train_y, K, M, Lw=Lw, cstr=cstr,
                                                    minSize=minSize, dropTh=dropTh)

    smogllim_pred = smogllim_inverse_map(yapp, smogllim_result$theta)
    smogllim_pred_t = smogllim_pred$x_exp[1:Lt, , drop=FALSE]

    smogllim_train_diff = train_t - smogllim_pred_t[, train_index, drop=FALSE]
    smogllim_train_mse = mean(apply(smogllim_train_diff^2, 2, sum))
    smogllim_test_diff = test_t - smogllim_pred_t[, test_index, drop=FALSE]
    smogllim_test_mse = mean(apply(smogllim_test_diff^2, 2, sum))
    cat(sprintf("SMoGLLiM Train MSE: %.4f, test MSE: %.4f\n",
                                            smogllim_train_mse, smogllim_test_mse))
    save(list = ls(all.names = TRUE), file = save_name, envir =
  environment())
}
