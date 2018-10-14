smogllim_remove_empty_clusters = function(th1, cstr1, r){
    K = nrow(th1$rho)
    M = ncol(th1$rho)
    ec = array(TRUE, c(K, M)) #% false if component k is empty.
    for(k in 1:K) {
        for(l in 1:M) {
            if(round(sum(r[,k,l])) == 0 || !is.finite(sum(r[, k, l]))) {
              ec[k, l]=FALSE
            }
        }
    }

    #global cluster i would be removed if sumEC[i] = 0
    sumEC = apply(ec, 1, sum)
    validClust = sumEC > 0
    r = r[, validClust, , drop=FALSE]
    r = round(r, 8)
    # #dont do reinit at this point
    # if (sum(ec)==0)
    #    {print('REINIT! ')}
    # #    r = emgm(rbind(tapp,yapp), K, 2, verb)$R;
    # #    ec=rep(TRUE,ncol(r));} else {r=r[,ec];}


    th = th1
    cstr = cstr1
    temp_ec = ec
    ec = ec[, 1]
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
    return(list(th=th, cstr=cstr, r=r))
}
