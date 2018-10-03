context("Testing for development procedure")
load(file.path('..', 'gen_test_data', 'test_data.RData'))
test_that("timesTwo, Rcpp", {
    expect_equal(timesTwo(2), 4)})

test_that("cpp_logsumexp", {
    expect_equal(logsumexp_output1, cpp_logsumexp(logsumexp_input1, 1))
    expect_equal(logsumexp_output2, cpp_logsumexp(logsumexp_input2, 1))
    })

test_that("cpp_loggausspdf", {
    expect_equal(loggausspdf_output1, cpp_loggausspdf(loggausspdf_input1_x,
                                  loggausspdf_input1_mu,
                                  loggausspdf_input1_Sigma))
    expect_equal(loggausspdf_output2, cpp_loggausspdf(loggausspdf_input2_x,
                                  loggausspdf_input2_mu,
                                  loggausspdf_input2_Sigma))
    })
