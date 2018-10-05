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
            cstr$cw=cstr$cw[, ec, , drop=FALSE]
        if(!is.null(cstr$Gammaw) && !is.character(cstr$Gammaw))
            cstr$Gammaw = cstr$Gammaw[, , ec, , drop=FALSE]
        th$rho = th$rho[ec, , drop=FALSE]
        th$c = th$c[, ec, , drop=FALSE]
        th$Gamma = th$Gamma[, , ec, , drop=FALSE]
        th$A = th$A[, , ec, , drop=FALSE]
        th$b = th$b[, ec, , drop=FALSE]
        th$Sigma = th$Sigma[, , ec, drop=FALSE]
    }
    return(list(th=th, cstr=cstr))
}
