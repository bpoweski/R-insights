fixture <- function(name) {
    path <- file.path("data", name)
    return(readChar(path, file.info(path)$size))
}

context("events")

test_that("returns a data.table of events", {
    response <- parse_insights(fixture("math_expression.json"))
    expect_equal(names(response), c("duration - databaseDuration", "timestamp"))
    expect_equal(response[1]$timestamp, as.POSIXct(1460758887377/1000, origin = "1970-01-01", tz = "UTC"))
    expect_equal(nrow(response), 10)
})

test_that("events with aliases name columns", {
    response <- parse_insights(fixture("math_expression_alias.json"))
    expect_equal(names(response), c("app_duration", "timestamp"))
    expect_equal(nrow(response), 1)
})

test_that("events without an event object", {
    response <- parse_insights(fixture("events_without_event.json"))
    expect_equal(names(response), c("duration - databaseDuration", "timestamp"))
    expect_equal(response[1]$timestamp, as.POSIXct(1460758887377/1000, origin = "1970-01-01", tz = "UTC"))
    expect_equal(nrow(response), 1)
})

context("aggregate functions")

test_that("a list is returned with aggregations as named values", {
    response <- parse_insights(fixture("aggregates.json"))
    expect_equal(names(response), c("uniquecount.userAgentVersion", "count.userAgentName"))
    expect_equal(response$count, 16443966)
})

test_that("aliased aggregations are preferred", {
    response <- parse_insights(fixture("count_alias.json"))
    expect_equal(response$foo, 6826322)
})

test_that("reused functions named different where possible", {
    response <- parse_insights(fixture("mixed_use_same_function.json"))
    expect_equal(length(response), 4)
    expect_equal(response$count.databaseDuration, 3236408)
    expect_equal(response$count.name, 6787668)
    expect_equal(response$bar, 6787668)
    expect_equal(response$count, 6787668)
})

context("facets")

test_that("facet names are columns", {
    response <- parse_insights(fixture("facet_alias.json"))
    expect_equal(nrow(response), 7)
    expect_equal(length(response), 3)
    expect_equal(response$n, c(333, 12, 15, 12, 6, 384, 387))
})

test_that("facet names are columns and unknown", {
    response <- parse_insights(fixture("facet_alias.json"), include.unknown = TRUE)
    expect_equal(nrow(response), 8)
    expect_equal(length(response), 3)
})
