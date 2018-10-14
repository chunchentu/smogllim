library(abind)
library(MASS)
smogllim = function(tapp, yapp, in_K, in_M, in_r=NULL, maxiter=100, Lw=0, cstr=NULL,
                    minSize=0, dropTh=Inf, orig_th=NULL) {
    Lt = nrow(tapp)
    L = Lt + Lw
    D = nrow(yapp)
    N = ncol(yapp)
    #initialize cstr
    if(! "ct" %in% names(cstr)) cstr$ct = NULL
    if(! "cw" %in% names(cstr)) cstr$cw = NULL
    if(! "Gammat" %in% names(cstr)) cstr$Gammat = NULL
    if(! "Gammaw" %in% names(cstr)) cstr$Gammaw = NULL
    if(! "pi" %in% names(cstr)) cstr$pi = NULL
    if(! "A" %in% names(cstr)) cstr$A = NULL
    if(! "b" %in% names(cstr)) cstr$b = NULL
    if(! "Sigma" %in% names(cstr)) cstr$Sigma = "*"

    if(is.null(in_r)){
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
            temp_cluster = Mclust(t(temp_t), M, verbose=FALSE)
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
    } else {
        r = in_r
    }
    if(ncol(tapp) != ncol(yapp)) {
        stop("Observations must be in columns and variables in rows")
    }



    if(Lw==0) {
        Sw = NULL
        muw = NULL
    } else {
        theta = smogllim_Maximization(tapp, yapp, r, NULL, NULL, cstr)

        K = nrow(theta$rho)
        M = ncol(theta$rho)
        if(is.null(cstr$cw)) {
            cstr$cw=array(0,c(Lw,K,M))
        }
        theta$c = abind(theta$c, cstr$cw, along=1)

        Gammaf = array(0, c(L, L, K, M))
        Gammaf[1:Lt, 1:Lt, , ] = theta$Gamma
        if(is.null(cstr$Gammaw)) {
            cstr$Gammaw = array(diag(Lw), c(Lw, Lw, K, M))
        }
        Gammaf[(Lt+1):L, (Lt+1):L, , ] = cstr$Gammaw
        theta$Gamma = Gammaf

        # % Initialize Awk with local weighted PCAs on residuals:
        Aw = array(0, c(D, Lw, K))
        for(k in 1:K) {
            rk_bar = sum(r[, k, ])
            if(round(rk_bar) == 0) next
            rk = apply(r[, k, ], 1, sum)
            tempReg = array(0, c(D, N))
            for(l in 1:M){
                Akl = theta$A[, , k, l]
                bkl = theta$b[, k, l]
                if(sum(r[, k, l]) > 0) {
                    temp = sweep(Akl%*%tapp, 1, bkl, "+")
                    tempReg = tempReg + sweep(temp, 2, r[, k, l], '*')
                }
            }
            tempReg = sweep(tempReg, 2, rk, "/")
            tempReg[is.nan(tempReg)] = 0

            w = yapp - tempReg
            w = sweep(w, 2, sqrt(rk/rk_bar), "*")
            C = tcrossprod(w) # Residual weighted covariance matrix
            tmp = eigen(C)
           	U = tmp$vectors[, 1:Lw]
          	Lambda = tmp$values[1:Lw]
            sigma2k = (sum(diag(C))-sum(Lambda))/(D-Lw) + 1e-12 #scalar

            theta$Sigma[, , k] = sigma2k * diag(D)
            temp_L = diag(Lambda + 2e-8)
            Aw[, , k] = U %*% sqrt(temp_L - sigma2k*diag(Lw))
        }

        theta$A = abind(theta$A, array(Aw, c(D, Lw, K, M)), along=2)

        tmp = smogllim_ExpectationZU(tapp, yapp, theta)
        r = tmp$r
        tmp = smogllim_remove_empty_clusters(theta, cstr, r)
        theta = tmp$th
        cstr = tmp$cstr
        r = tmp$r

        tmp = smogllim_ExpectationW(tapp, yapp, theta, r)
        muw = tmp$muw
        Sw = tmp$Sw
    }
    # LL = array(-Inf, maxiter)
    # iter = 0
    # converged = FALSE
    # while( !converged & iter<maxiter) {
    #     iter = iter + 1
    #     theta = smogllim_Maximization(tapp, yapp, r, muw, Sw, cstr)
    #     tmp = smogllim_ExpectationZU(tapp, yapp, theta, NULL)
    #     r = tmp$r
    #     LL[iter] = tmp$LL
    # 	tmp = smogllim_remove_empty_clusters(theta, cstr, r)
    # 	theta = tmp$th
    # 	cstr = tmp$cstr
    # 	r = tmp$r
    #
    #     tmp = smogllim_ExpectationW(tapp, yapp, theta, r)
    #     muw = tmp$muw
    #     Sw = tmp$Sw
    #
    #     if(iter>=5){
    #         deltaLL_total = max(LL[1:iter]) - min(LL[1:iter])
    #         deltaLL = LL[iter] - LL[iter-1]
    #         converged = (deltaLL <= (0.001*deltaLL_total))
    # 	}
    #
    # }

    # enhance training

    LL = array(-Inf, maxiter)
    iter = 0
    converged = FALSE
    # temp_pred = smogllim_inverse_map(yapp, theta)
    # proj = temp_pred$proj[1:Lt, , , ]
    dropID = NULL
    while( !converged & iter<maxiter) {
        iter = iter + 1

        tmp = smogllim_ExpectationZU(tapp, yapp, theta, dropID=dropID)
        r = tmp$r
        LL[iter] = tmp$LL
        temp_pred = smogllim_inverse_map(yapp, theta)
        pred = temp_pred$x_exp[1:Lt, , drop=FALSE]
        proj = temp_pred$proj[1:Lt, , , ,drop=FALSE]
        pred_SE = apply((pred - tapp)^2, 2, sum)
        dropID = which(pred_SE > dropTh)
        r[dropID, , ] = 0

    	# reassign
        # iterate through cluster with clsuter size smaller than minSize
    	all_cluster_weight = apply(r, 2:3, sum)
    	K = dim(all_cluster_weight)[1]
    	M = dim(all_cluster_weight)[2]
    	proj_se = apply(sweep(proj, 1:2, tapp, "-")^2, 2:4, sum)
    	for(cluster_weight in 0:(minSize-1)){
    	    all_ind = which(all_cluster_weight >= cluster_weight &
    	                    all_cluster_weight < (cluster_weight+1))
    	    for(ind in all_ind){
                k = ((ind-1) %% K) + 1
                l = floor((ind-1) / K) + 1
                proj_se[, k, l] = Inf
                # data belongs to cluster (k, l)
                d_index = which(r[, k, l] > 0)
                for(temp_index in d_index){
                    # find the cluster with smallest prediction error
                    # drop if the smallest predcition error is less than dropTh
                    temp_index_se = proj_se[temp_index, , ]
                    temp_ind = which.min(temp_index_se)
                    if(temp_index_se[temp_ind] > dropTh){
                        dropID = c(dropID, temp_index)
                        next
                    }
                    temp_k = ((temp_ind-1) %% K) + 1
                    temp_l = floor((temp_ind-1) / K) + 1
                    r[temp_index, , ] = 0
                    r[temp_index, temp_k, temp_l] = 1
                }
    	    }
    	    all_cluster_weight = apply(r, 2:3, sum)
    	}

        tmp = smogllim_remove_empty_clusters(theta, cstr, r)
    	theta = tmp$th
    	cstr = tmp$cstr
    	r = tmp$r

        tmp = smogllim_ExpectationW(tapp, yapp, theta, r)
        muw = tmp$muw
        Sw = tmp$Sw
        theta = smogllim_Maximization(tapp, yapp, r, muw, Sw, cstr, minSize, dropID)

        # calculate in-sample prediction errors
        # temp = smogllim_inverse_map(yapp, theta)
        # pred = temp$x_exp[1:Lt, , drop=FALSE]
        # pred_SE = apply((pred - tapp)^2, 2, sum)
        # dropID = which(pred_SE > dropTh)
        # r[dropID, , ] = 0

        # temp_list = NULL
        # if(minSize >0){
        #     for(k in 1:dim(r)[2]) {
        #         for(l in 1:dim(r)[3]){
        #             if(sum(r[, k, l])<minSize){
        #                 theta$c[, k, l] = NaN
        #             }
        #         }
        #     }
        # }


        if(iter>=3){
            deltaLL_total = max(LL[1:iter]) - min(LL[1:iter])
            deltaLL = LL[iter] - LL[iter-1]
            converged = (deltaLL <= (0.001*deltaLL_total))
    	}

    }

    LLf=LL[iter]

    return(list(theta=theta, LL=LLf, dropID=dropID))
}
