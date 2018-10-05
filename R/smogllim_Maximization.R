smogllim_Maximization = function(tapp, yapp, r, muw, Sw, cstr, minSize=0, dropID=NULL) {

    K = dim(r)[2]
    M = dim(r)[3]
    D = nrow(yapp)
    N = ncol(yapp)
    Lt = nrow(tapp)
    Lw = ifelse(is.null(muw), 0, nrow(muw))
    L = Lt + Lw
    th = list()
    th$c = array(NaN, c(L, K, M))
    th$Gamma = array(0, c(L, L, K, M))
    th$rho = array(NaN, c(K, M))
    th$A = array(NaN, c(D, L, K, M))
    th$b = array(NaN, c(D, K, M))
    th$Sigma = array(NaN,c(D, D, K))
    th$Lt = Lt


    if(Lw>0) {
        th$c[(Lt+1):L, , ] = cstr$cw
        th$Gamma[(Lt+1):L, (Lt+1):L, , ] = cstr$Gammaw
    }
    rk_bar = array(0, K)
    rkl_bar = array(0, c(K, M))


    for (k in 1:K){

        #  % Posteriors' sums
        rk = apply(r[, k, ], 1, sum)  #% 1xN
        rk_bar[k] = sum(rk) # 1x1
        rnk = apply(r[, k, ], 1, sum)
        if(Lw>0) {
            x = rbind(tapp, muw[, , k])
            Skx = rbind(cbind(matrix(0, Lt, Lt), matrix(0, Lt, Lw)),
                        cbind(matrix(0, Lw, Lt), Sw[, , k]))

            #calculate the global center
            yk_bar = rowSums(sweep(yapp, 2, rk, "*"))/rk_bar[k]

            weights = sqrt(rk)
            y_stark = sweep(yapp, 1, yk_bar, "-")
            y_stark = sweep(y_stark, 2, weights, "*")

            xk_bar= rowSums(sweep(x, 2, rk, "*"))/rk_bar[k]
            x_stark = sweep(x, 1, xk_bar, "-")
            x_stark = sweep(x_stark, 2, weights, "*")


        #% Robustly compute optimal transformation matrix Ak
        if(!all(Skx==0))
        {
            if(N>=L & det(Skx+tcrossprod(x_stark)) >10^(-8)){
            A = tcrossprod(y_stark,x_stark) %*% qr.solve(Skx+tcrossprod(x_stark))
            } else {
            A = tcrossprod(y_stark,x_stark) %*% ginv(Skx+tcrossprod(x_stark))
            }
            Akw = A[, (Lt+1):L]
        } else if(!all(x_stark==0)){
            if(N>=L & det(tcrossprod(x_stark))>10^(-8)) {
            A = tcrossprod(y_stark,x_stark) %*% qr.solve(tcrossprod(x_stark))
            } else if (N<L && det(crossprod(x_stark))>10^(-8)) {
            A = y_stark %*% solve(crossprod(x_stark)) %*% t(x_stark)
            }  else {
            A = y_stark %*% ginv(x_stark)  # DxL
            }
            Akw = A[, (Lt+1):L]
        } else {# Correspond to null variance in cluster k or L=0:
            Akw = 0 # DxL
        }
        newy = yapp - Akw %*% muw[, , k]
    } else {
        newy = yapp
    } # end of if(Lw>0) Ln:35

    temp_reg = array(0, c(D, N))
    for(l in 1:M){
        rkl = r[, k, l]
        rkl_bar[k, l] = sum(rkl)

        if(round(rkl_bar[k, l]) < minSize || round(rkl_bar[k, l])==0) {
            th$c[, k, l] = NaN
            next
        }


        th$c[1:Lt, k, l] = rowSums(sweep(tapp, 2, rkl, "*")) / rkl_bar[k, l]
        diffGamma = sweep(sweep(tapp, 1, th$c[1:Lt, k, l], "-"), 2,
                          sqrt(rkl), "*")
        th$Gamma[1:Lt, 1:Lt, k, l] = tcrossprod(diffGamma) / rkl_bar[k, l];

        # Compute optimal weight pik
        th$rho[k, l] = rkl_bar[k, l]/(N - length(dropID))

        x = tapp # LtxN

        yk_bar = rowSums(sweep(newy, 2, rkl, "*"))/rkl_bar[k, l] # Dx1
        xk_bar = rowSums(sweep(x, 2, rkl, "*"))/rkl_bar[k, l] # Lx1

        # Compute weighted, mean centered y and x
        weights = sqrt(rkl) # 1xN
        y_stark = sweep(newy, 1, yk_bar, "-") # DxN
        y_stark = sweep(y_stark, 2, weights, "*") # DxN

        x_stark = sweep(x, 1, xk_bar, "-") # LxN
        x_stark = sweep(x_stark, 2, weights, "*") # LxN

        # browser()
        if(!all(x_stark==0)) {
            if(N>=L & det(tcrossprod(x_stark))>10^(-8)) {
                th$A[, 1:Lt, k, l] = tcrossprod(y_stark,x_stark) %*%
                                                qr.solve(tcrossprod(x_stark))
          } else if (N<L && det(crossprod(x_stark))>10^(-8)) {
                th$A[, 1:Lt, k, l] = y_stark %*% solve(crossprod(x_stark)) %*%
                                        t(x_stark)
          } else {
            th$A[, 1:Lt, k, l] = y_stark %*% ginv(x_stark)
          }
        } else {
            th$A[, 1:Lt, k, l] = 0
        }

        if(Lw>0){
            th$A[, (Lt+1):L, k, l] = Akw
        }

        if(Lw>0) {
            wk = yapp - th$A[, 1:Lt, k, l] %*% x - Akw %*% muw[, , k]
        } else {
            wk = yapp - th$A[, 1:Lt, k, l] %*% x
        }
        th$b[, k, l] = rowSums(sweep(wk, 2, rkl, "*"))/rkl_bar[k, l]

        #accumulate over residuals
        temp = sweep(th$A[, 1:Lt, k, l] %*% x, 1, th$b[, k, l], "+")
        temp_reg = temp_reg + sweep(temp, 2, r[, k, l], "*")

        if(!is.finite(sum(th$Gamma[1:Lt, 1:Lt, k, l]))) {
          th$Gamma[1:Lt, 1:Lt, k, l]=0
        }
        th$Gamma[1:Lt, 1:Lt, k, l] = th$Gamma[1:Lt, 1:Lt, k, l] + 1e-8*diag(Lt)
    } #end of for l in 1:M

    # Compute optimal covariance matrix Sigmak
    if(Lw>0) {
        Swk = Sw[, , k]
        ASAwk = Akw %*% tcrossprod(Swk, Akw)
    } else {
        ASAwk = 0
    }

    temp = sweep(temp_reg, 2, rnk, "/")
    temp[is.nan(temp)] = 0
    temp_reg = temp
    if(Lw>0){
        wk = yapp - temp_reg - Akw%*%muw[,,k]
    } else {
        wk = yapp - temp_reg
    }

    diffSigma = sweep(wk, 2, sqrt(rk), "*")
    if(cstr$Sigma %in% c("","*")) {
        # Full Sigma
        th$Sigma[, , k] = tcrossprod(diffSigma)/rk_bar[k]
        th$Sigma[, , k] = th$Sigma[, , k] + ASAwk
    } else {
        if(substr(cstr$Sigma, 1, 1)=="d" ||
           substr(cstr$Sigma[1], 1, 1)=="i") {
            #% Diagonal terms
            sigma2 = rowSums(diffSigma^2)/rk_bar[k]
            if(substr(cstr$Sigma[1], 1, 1)=="d") {
              # Diagonal Sigma
              th$Sigma[, , k] = diag(sigma2, ncol=D, nrow=D) #% DxD
              if (is.null(dim(ASAwk))) {
                th$Sigma[, , k] = th$Sigma[, , k] + diag(ASAwk, ncol=D, nrow=D)
              } else {
                th$Sigma[, , k] = th$Sigma[, , k] + diag(diag(ASAwk))
              }
            } else {
              # Isotropic Sigma
              th$Sigma[, , k] = mean(sigma2) * diag(D) # DxD
              if(is.null(dim(ASAwk))) {
                th$Sigma[, , k] = th$Sigma[, , k] +
                    sum(diag(ASAwk, ncol=D, nrow=D))/D*diag(D)
              } else {
                th$Sigma[, , k] = th$Sigma[, , k] + sum(diag(ASAwk))/D*diag(D)
              }
            }
      } else {
        cstr$Sigma
        stop('  ERROR: invalid constraint on Sigma.')
      }
    }

    #% Avoid numerical problems on covariances:

    if(! is.finite(sum(th$Sigma[, , k]))) {
        th$Sigma[, , k] = 0
    }
    th$Sigma[, , k] = th$Sigma[, , k] + 1e-8*diag(D)

    } #end of for k

    if(substr(cstr$Sigma, 2, 2)=="*") {
        #%%% Equality constraint on Sigma
        th$Sigma = sweep(th$Sigma, 3, rk_bar, "*")
        tempMat = apply(th$Sigma, 1:2, sum)/(N-length(dropID))
        th$Sigma = array(tempMat, dim=c(D, D, K))
    }
    return(th)
}
