smogllim_remove_empty_clusters = function(th1, cstr1, temp_ec){
    th = th1
    cstr = cstr1
    ec = temp_ec[, 1]
    M = dim(temp_ec)[2]
    for(i in 2:M){
        ec = ec | temp_ec[, i]
    }
    if(sum(ec) != length(ec)){
        if(!is.null(cstr$cw) && !is.character(cstr$cw))
            cstr$cw=cstr$cw[, ec, ]
        if(!is.null(cstr$Gammaw) && !is.character(cstr$Gammaw))
            cstr$Gammaw = cstr$Gammaw[, , ec, ]
        th$rho = th$rho[ec, ]
        th$c = th$c[, ec, ]
        th$Gamma = th$Gamma[, , ec, ]
        th$A = th$A[, , ec, ]
        th$b = th$b[, ec, ]
        th$Sigma = th$Sigma[, , ec]
    }
    return(list(th=th, cstr=cstr))
}
