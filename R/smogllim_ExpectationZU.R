smogllim_ExpectationZU = function(tapp, yapp, th, dropID=NULL){

    D = nrow(yapp)
    N = ncol(yapp)
    K = nrow(th$rho)
    M = ncol(th$rho)
    Lt = nrow(tapp)
    L = nrow(th$c)
    Lw = L - Lt

    logr = array(NaN, c(N, K, M))

    for(k in 1:K){
        for(l in 1:M){

        #check if the local cluster is empty
        #use c as an indicator, if c is Nan, this local cluster is empty
        if(any(is.nan(th$c[1:Lt, k, l]))) {
            logr[,k,l] = -Inf
            next
        }

        muyk = th$b[, k, l, drop=FALSE] # Dx1
        covyk = th$Sigma[, , k] # DxD
        if(Lt > 0) {
            if(L == 1) {
              Atk = th$A[, 1:Lt , k, l, drop=FALSE]
          } else {
              Atk = th$A[ , 1:Lt, k, l]} # DxLt
          muyk = sweep(Atk %*% tapp, 1, muyk, "+") # DxN
        }

        if(Lw > 0) {
            Awk = matrix(th$A[, (Lt+1):L, k, 1, drop=FALSE], ncol=Lw, nrow=D)
            Gammawk = th$Gamma[(Lt+1):L, (Lt+1):L, k, l] # LwxLw
            cwk = th$c[(Lt+1):L, k, l] # Lwx1
            covyk = covyk + Awk %*% Gammawk %*%t (Awk) # DxD
            muyk = sweep(muyk, 1, Awk%*%cwk, "+") #% DxN
        }
        logr[, k, l] = log(th$rho[k, l])
        logr[, k, l] = logr[, k, l] + loggausspdf(yapp, muyk, covyk)
        if(Lt > 0) {
            logr[, k, l] = logr[, k, l]+ loggausspdf(tapp,
                                        th$c[1:Lt,k,l,drop=FALSE],
                                        as.matrix(th$Gamma[1:Lt, 1:Lt, k, l]))
        }
      }
    }

    lognormr = logsumexp(logr, 2)
    temp_lognormr = lognormr
    temp_lognormr[dropID] = 0
    LL = sum(temp_lognormr)
    #need to reshape the size to make sweep work
    logr_reshape = matrix(logr, nrow=N, ncol=K*M)
    log_posProb = sweep(logr_reshape, 1, lognormr, "-")
    r = array(exp(log_posProb), c(N,K,M))
    r[dropID, , ] = 0
    r = round(r, 8)
    # remove empty clusters only when the whole global cluster is empty
    ec = array(TRUE, c(K, M)) #% false if component k is empty.
    for(k in 1:K) {
        for(l in 1:M) {
            if(round(sum(r[,k,l])) == 0 || !is.finite(sum(r[, k, l]))) {
              ec[k,l]=FALSE
            }
        }
    }

    #global cluster i would be removed if sumEC[i] = 0
    sumEC = apply(ec, 1, sum)
    validClust = sumEC > 0
    r = r[, validClust, , drop=FALSE]
    r = round(r, 8)
    #dont do reinit at this point
    if (sum(ec)==0){
        print('REINIT! ')
        gllim_result = gllim(tapp, yapp, K, Lw=Lw, cstr=cstr)
        gllim_r = round(gllim_result$r)

    cluster_assign = apply(gllim_r, 1, which.max)
    temp_r = array(0, c(N, 2))
    for(c in sort(unique(cluster_assign))){
        index = which(cluster_assign==c)
        if(length(index)<=M) {
            temp_r[index, 1] = c
            temp_r[index, 2] = 1
        } else {
            temp_t = tapp[, index, drop=FALSE]
            temp_cluster = Mclust(t(temp_t), 1:M, verbose=FALSE)
            if(is.null(temp_cluster)){
                temp_r[index, 1] = c
                temp_r[index, 2] = 1
            } else {
                temp_assign = temp_cluster$classification
                temp_r[index, 1] = c
                temp_r[index, 2] = temp_assign
            }
        }
    }

    r = array(0, c(N, K, M))
    for(i in 1:N){
        r[i, temp_r[i, 1], temp_r[i, 2]] = 1
    }
    ec = array(TRUE, c(K, M))


        }
    #    r = emgm(rbind(tapp,yapp), K, 2, verb)$R;
    #    ec=rep(TRUE,ncol(r));} else {r=r[,ec];}
    return(list(r=r, LL=LL, ec=ec))
}
