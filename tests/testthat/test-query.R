context("query building")

test_that("nrql_quote wraps single quotes", {    
    expect_equal(nrql_quote("timestamp"), "'timestamp'")
})
