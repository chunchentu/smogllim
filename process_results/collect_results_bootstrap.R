data_root = "/scratch/stats_flux/timtu/smogllim/bootstrap"
file_list = dir(data_root)
file_len = length(file_list)


smogllim_df = NULL
gllim_df = NULL

for(file_index in 1:file_len){
    d = new.env()
    load(file.path(data_root, file_list[file_index]), envir=d)
    cat(sprintf("(%d/%d) SMoGLLiM: cvID:%d K: %d, Lw: %d, M: %d, minSize: %d, dropTh: %.2f\n",
        file_index, file_len, d$cvID, d$K, d$Lw, d$M, d$minSize, d$dropTh))
    train_num = dim(d$train_y)[2]
    data_type = array("train", train_num)
    smogllim_sse = apply(d$smogllim_train_diff^2, 2, sum)
    temp_smogllim_df = data.frame(cvID=d$cvID,
                                  img_size=d$target_size, 
                                  data_type=data_type,
                                  index=d$train_index,
                                  K=d$K,
                                  Lw=d$Lw,
                                  M=d$M,
                                  minSize=d$minSize,
                                  dropTh=d$dropTh,
                                  SSE=smogllim_sse
                                  )
    smogllim_df = rbind(smogllim_df, temp_smogllim_df)

    gllim_sse = apply(d$gllim_train_diff^2, 2, sum)
    temp_gllim_df = data.frame(cvID=d$cvID, 
                                img_size=d$target_size, 
                                  data_type=data_type,
                                  index=d$train_index,
                                  K=d$K,
                                  Lw=d$Lw,
                                  SSE=gllim_sse
                                  )
    gllim_df = rbind(gllim_df, temp_gllim_df)



    test_num = dim(d$test_y)[2]
    data_type = array("test", test_num)
    smogllim_sse = apply(d$smogllim_test_diff^2, 2, sum)
    temp_smogllim_df = data.frame(cvID=d$cvID, 
                                  img_size=d$target_size, 
                                  data_type=data_type,
                                  index=d$test_index,
                                  K=d$K,
                                  Lw=d$Lw,
                                  M=d$M,
                                  minSize=d$minSize,
                                  dropTh=d$dropTh,
                                  SSE=smogllim_sse
                                  )
    smogllim_df = rbind(smogllim_df, temp_smogllim_df)

    gllim_sse = apply(d$gllim_test_diff^2, 2, sum)
    temp_gllim_df = data.frame(cvID=d$cvID, 
                               img_size=d$target_size, 
                                  data_type=data_type,
                                  index=d$test_index,
                                  K=d$K,
                                  Lw=d$Lw,
                                  SSE=gllim_sse
                                  )
    gllim_df = rbind(gllim_df, temp_gllim_df)
}
save.image("face_bootstrap_results.Rdata")