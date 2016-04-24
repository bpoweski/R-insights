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

test_that("an empty data.table is returned when there are no matched facets", {
    response <- parse_insights(fixture("facet_no_match.json"))
    expect_equal(length(response), 0)
    expect_equal(nrow(response), 0)
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

context("timeseries")

test_that("time series to table", {
    dt <- timeseries_table(list(results = list(list(count = 167)), beginTimeSeconds = 1461420019, endTimeSeconds = 1461427219))
    expect_equal(nrow(dt), 1)
})

test_that("simple timeseries", {
    response <- parse_insights(fixture("timeseries_simple.json"))
    expect_equal(nrow(response), 1)
    expect_equal(response[, count], 167)
})

test_that("multiple timeseries values", {
    response <- parse_insights(fixture("timeseries_multiple_values.json"))
    expect_equal(nrow(response), 2)
    expect_equal(names(response), c("begin_time", "end_time", "count", "unique hosts"))
    expect_equal(response[1:2, count], c(83, 83))
    expect_equivalent(response[1, begin_time], as.POSIXct(1461423217, origin = "1970-01-01", tz = "UTC"))
    expect_equivalent(response[2, begin_time], as.POSIXct(1461426817, origin = "1970-01-01", tz = "UTC"))
})
