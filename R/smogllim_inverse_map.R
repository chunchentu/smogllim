smogllim_inverse_map = function(y, th){
	D = nrow(y)
	N = ncol(y)
	L = nrow(th$c)
	K = nrow(th$rho)
	M = ncol(th$rho)

    proj = array(NaN, c(L, N, K, M))
    logalpha = array(0, c(N, K, M))
    for(k in 1:K) {
        for(l in 1:M) {
            if(L==1) {
                Akl = th$A[, , k, l, drop=FALSE]
            } else {
                Akl = th$A[, , k, l]
            } # DxL
            bkl = th$b[, k, l] # Dx1
            Sigmak = th$Sigma[, , k] # DxD
            if(L==1) {
                ckl = th$c[, k, l, drop=FALSE]
            } else {
                ckl = th$c[, k, l]
            } # Lx1
            Gammakl = th$Gamma[, , k, l] # LxL

            rhokl = th$rho[k, l]

            if(any(is.nan(ckl))) {
              logalpha[, k, l] = -Inf
              proj[, , k, l] = 0
              next
            }

            ckls=Akl%*%ckl+bkl; ## OK

            Gammakls = Sigmak + Akl %*% tcrossprod(Gammakl, Akl)

            iSk = solve(Sigmak)
            invSigmaks2 = diag(L) + Gammakl %*% crossprod(Akl, iSk) %*% Akl

            Akls = solve(invSigmaks2, Gammakl) %*% crossprod(Akl, iSk)

            bkls= solve(invSigmaks2) %*% (ckl-Gammakl%*%crossprod(Akl, iSk%*%bkl))

            proj[, , k, l] = sweep(Akls %*% y, 1, bkls, "+")

            logalpha[, k, l] = log(rhokl) + loggausspdf(y, ckls, Gammakls)
      }
    }

    den = logsumexp(logalpha, 1)
    logalpha_reshape = matrix(logalpha, nrow=N, ncol=K*M)
    log_posProb = sweep(logalpha_reshape, 1, den,"-")
    alpha = array(exp(log_posProb), c(N, K, M))

    temp1 = sweep(proj, 2:4, alpha, "*")
    temp2 = apply(temp1, 1:2, sum)
    x_exp = array(temp2, c(L, N))

    return(list(x_exp=x_exp, alpha=alpha))
}
