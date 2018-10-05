data_root = "/scratch/stats_flux/timtu/smogllim/OJ"
file_list = dir(data_root)
file_len = length(file_list)


smogllim_df = NULL
gllim_df = NULL
gllim_process_list = NULL


for(file_index in 1:file_len){
    d = new.env()
    load(file.path(data_root, file_list[file_index]), envir=d)
    cat(sprintf("(%d/%d) SMoGLLiM: cvID:%d K: %d, Lw: %d, M: %d, minSize: %d, dropTh: %.2f\n",
        file_index, file_len, d$cvID, d$K, d$Lw, d$M, d$minSize, d$dropTh))
    tapp = d$tapp
    data_type = array("train", d$N)
    data_type[d$test_index] = "test"
    smogllim_SE = (d$tapp - d$smogllim_pred_t)^2
    temp_smogllim_df = data.frame(cvID=d$cvID, 
                                  data_type=data_type,
                                  index=1:d$N,
                                  K=d$K,
                                  Lw=d$Lw,
                                  M=d$M,
                                  minSize=d$minSize,
                                  dropTh=d$dropTh,
                                  pred=t(d$smogllim_pred_t),
                                  SE=t(smogllim_SE)
                                  )
    smogllim_df = rbind(smogllim_df, temp_smogllim_df)

    # save gllim results if not processed yet
    set_str = sprintf("cvID%d_K%d_Lw%d", d$cvID, d$K, d$Lw)
    if(! set_str %in% gllim_process_list){
        gllim_process_list = c(gllim_process_list, set_str)
        cat(sprintf("       GLLiM: %s\n", set_str))
        gllim_SE = (d$tapp - d$gllim_pred_t)^2
        temp_gllim_df = data.frame(cvID=d$cvID, 
                                   data_type=data_type,
                                   index=1:d$N,
                                   K=d$K,
                                   Lw=d$Lw,
                                   pred=t(d$gllim_pred_t),
                                   SE=t(gllim_SE)
                                   )
        gllim_df = rbind(gllim_df, temp_gllim_df)
    }
}
save.image("OJ_results.Rdata")