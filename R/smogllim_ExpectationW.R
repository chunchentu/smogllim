smogllim_ExpectationW = function(tapp, yapp, th, r, minSize=0){
    D = nrow(yapp)
    N = ncol(yapp)
    K = nrow(th$rho)
    M = ncol(th$rho)
    Lt = nrow(tapp)
    L = nrow(th$c)
    Lw = L - Lt
    if(Lw==0) {
        muw = NULL
        Sw = NULL
    } else {
        Sw = array(0, c(Lw, Lw, K))
        muw = array(0, c(Lw, N, K))
        for (k in 1:K) {
            Awk = th$A[, (Lt+1):L, k, 1]
            for (l in 1:M) {
                #skip if no smaples are assigned to cluster (k,l)
                if(round(sum(r[, k, l])) < minSize || round(sum(r[, k, l])) == 0) next

                Atk = th$A[, 1:Lt, k, l]
                Sigmak = th$Sigma[, , k]

                Gammawk = th$Gamma[(Lt+1):L, (Lt+1):L, k, l]
                cwk = th$c[(Lt+1):L, k, l]
                invSwk = diag(Lw) + tcrossprod(Gammawk, Awk) %*%
                                                    solve(Sigmak, Awk)

                if (!is.null(tapp)){
                    Atkt = Atk %*% tapp
                } else {
                    Atkt=0
                }

                residual = sweep(yapp - Atkt, 1, th$b[, k, l], "-")
                muw_L = solve(invSwk,
                    sweep(tcrossprod(Gammawk,Awk) %*% solve(Sigmak, residual),
                            1, cwk, "+"))
                muw[, , k] = muw[, , k] + sweep(muw_L, 2, r[, k, l], "*")
            }
            Gammawk = th$Gamma[(Lt+1):L, (Lt+1):L, k, l]
            cwk = th$c[(Lt+1):L, k, l]
            invSwk = diag(Lw) + tcrossprod(Gammawk,Awk) %*% solve(Sigmak, Awk)
            Sw[, , k] = solve(invSwk, Gammawk)

            rnk = apply(r[, k, ], 1, sum)
            temp_muw = sweep(muw[, , k], 2, rnk, "/")
            temp_muw[is.nan(temp_muw)] = 0
            muw[, , k] = temp_muw
        }
    }
    return(list(muw=muw, Sw=Sw))
}
