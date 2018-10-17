library(R.matlab)
boostrap_NN_fun = function(cvID, save_prefix="."){

    test_num = 5

    data_path = "sim_data"
    target_size = 30
    full_data_path = file.path(data_path, sprintf('size%d.mat', target_size))
    temp = readMat(full_data_path)

    t1 = temp$subcluster1.t
    t2 = temp$subcluster2.t
    y1 = temp$subcluster1.y
    y2 = temp$subcluster2.y

    cluster_N = dim(t1)[2]

    set.seed(cvID)
    rand_index = sample(1:cluster_N)
    test_index = sort(rand_index[1:test_num])
    train_index = setdiff(1:cluster_N, test_index)

    train_t = cbind(t1[,train_index], t2[,train_index])
    train_y = cbind(y1[,train_index], y2[,train_index])

    test_t = cbind(t1[,test_index], t2[,test_index])
    test_y = cbind(y1[,test_index], y2[,test_index])


    cstr = list(Sigma="*")
    K = 2
    Lw = 9
    Lt = dim(train_t)[1]
    minSize = 5
    M = 2
    dropTh = 0.5
    # fit gllim model
    set.seed(cvID)
    in_r = array(0, c(cluster_N*2, K))
    in_r[1:cluster_N, 1] = 1
    in_r[(cluster_N+1):(cluster_N*2), 2] = 1
    in_r = list(R=in_r)

    gllim_result = gllim(train_t, train_y, K, in_r=in_r, Lw=Lw, cstr=cstr)
    gllim_train_pred = gllim_inverse_map(train_y, gllim_result)
    gllim_train_pred_t = gllim_train_pred$x_exp[1:Lt, , drop=FALSE]
    gllim_train_diff = train_t - gllim_train_pred_t
    gllim_train_mse = mean(apply(gllim_train_diff^2, 2, sum))

    gllim_test_pred = gllim_inverse_map(test_y, gllim_result)
    gllim_test_pred_t = gllim_test_pred$x_exp[1:Lt, , drop=FALSE]
    gllim_test_diff = test_t - gllim_test_pred_t
    gllim_test_mse = mean(apply(gllim_test_diff^2, 2, sum))
    cat(sprintf("GLLiM: Train MSE: %.4g, test MSE: %.4g\n",
                                                gllim_train_mse, gllim_test_mse))

    smogllim_result = smogllim(train_t, train_y, K, M, Lw=Lw, cstr=cstr,
                                                        minSize=minSize, dropTh=dropTh)

    smogllim_train_pred = smogllim_inverse_map(train_y, smogllim_result$theta)
    smogllim_train_pred_t = smogllim_train_pred$x_exp[1:Lt, , drop=FALSE]
    smogllim_train_diff = train_t - smogllim_train_pred_t
    smogllim_train_mse = mean(apply(smogllim_train_diff^2, 2, sum))


    smogllim_test_pred = smogllim_inverse_map(test_y, smogllim_result$theta)
    smogllim_test_pred_t = smogllim_test_pred$x_exp[1:Lt, , drop=FALSE]
    smogllim_test_diff = test_t - smogllim_test_pred_t
    smogllim_test_mse = mean(apply(smogllim_test_diff^2, 2, sum))


    cat(sprintf("SMoGLLiM Train MSE: %.4f, test MSE: %.4f\n",
                                            smogllim_train_mse, smogllim_test_mse))
    save_name = file.path(save_prefix, sprintf('cvID%d_size%d.Rdata'))
    save(list = ls(all.names = TRUE), file = save_name, envir =
      environment())
}
