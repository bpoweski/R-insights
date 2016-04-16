fixture <- function(name) {
    path <- system.file("tests", "testthat", "data", name, package = "insights")
    return(readChar(path, file.info(path)$size))
}

context("events")

test_that("returns a data.table of events", {
    response <- parse.insights(fixture("math_expression.json"))
    expect_equal(names(response), c("duration - databaseDuration", "timestamp"))
    expect_equal(response[1]$timestamp, as.POSIXct(1460758887377/1000, origin = "1970-01-01", tz = "UTC"))
    expect_equal(nrow(response), 10)
})

test_that("events with aliases name columns", {
    response <- parse.insights(fixture("math_expression_alias.json"))
    expect_equal(names(response), c("app_duration", "timestamp"))
    expect_equal(nrow(response), 1)
})

context("aggregate functions")

test_that("a list is returned with aggregations as named values", {
    response <- parse.insights(fixture("aggregates.json"))
    expect_equal(names(response), c("uniquecount.userAgentVersion", "count.userAgentName"))
    expect_equal(response$count, 16443966)
})

test_that("aliased aggregations are preferred", {
    response <- parse.insights(fixture("count_alias.json"))
    expect_equal(response$foo, 6826322)
})

test_that("reused functions named different where possible", {
    response <- parse.insights(fixture("mixed_use_same_function.json"))
    expect_equal(length(response), 4)
    expect_equal(response$count.databaseDuration, 3236408)
    expect_equal(response$count.name, 6787668)
    expect_equal(response$bar, 6787668)
    expect_equal(response$count, 6787668)
})

context("facets")

test_that("facet names are columns", {
    response <- parse.insights(fixture("facet_alias.json"))
    expect_equal(nrow(response), 8)
    expect_equal(length(response), 3)    
})
