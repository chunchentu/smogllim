loggausspdf = function(X, mu, Sigma){
	d = nrow(X) ; n = ncol(X) ;
	if (ncol(mu) == n)
	{X=X-mu;}#sweep(X,2,mu,"-")}
	else {X=sweep(X,1,mu,"-")}
	#X = #scale(X,center=mu,scale=FALSE) # d x n  ### X ou t(X)
	skip = tryCatch({
    U = chol(Sigma)
    skip = FALSE
  }, warning = function(msg){
   #cat("SNPD warning! ") 
   skip = TRUE
   return(skip)
  }, error = function(msg){
    skip = TRUE
    #cat("SNPD error!   " ) 
    
    return(skip)
  })
  if(skip==TRUE)
  {
    y = array(-Inf,n)
    return(y)
  } else {
  Q = solve(t(U),X)
  q = apply(Q,2,function(x){sum(x*x)})
  c = d*log(2*pi)+2*sum(log(diag(U)))
  y = -(c+q)/2
  return(y)
  }
}