library(xLLiM)
library(matrixcalc)
source("loggausspdf.R")
source("logsumexp.R")

set.seed(5566)

N = 100
K = 10
D = 20

# logsumexp
logsumexp_input1 = matrix(rnorm(N*K), nrow=N)
logsumexp_output1 = logsumexp(logsumexp_input1)
logsumexp_output1 = matrix(logsumexp_output1, ncol=1)

logsumexp_input2 = logsumexp_input1
logsumexp_input2[1, 3] = Inf
logsumexp_output2 = logsumexp(logsumexp_input2)
logsumexp_output2 = matrix(logsumexp_output2, ncol=1)


# loggausspdf
loggausspdf_input1_x = matrix(rnorm(D*N), nrow=D)
loggausspdf_input1_mu = matrix(rnorm(D*1), nrow=D)
S = matrix(rnorm(D*D), nrow=D)
loggausspdf_input1_Sigma = S%*%t(S)
loggausspdf_output1 = loggausspdf(loggausspdf_input1_x,
                                  loggausspdf_input1_mu,
                                  loggausspdf_input1_Sigma)
loggausspdf_output1 = matrix(loggausspdf_output1, nrow=1)


loggausspdf_input2_x = loggausspdf_input1_x
loggausspdf_input2_mu = loggausspdf_input1_mu
S = matrix(rnorm(D*D), nrow=D)
loggausspdf_input2_Sigma = S
loggausspdf_output2 = matrix(-Inf, nrow=1, ncol=N)


save.image(file="test_data.RData")
