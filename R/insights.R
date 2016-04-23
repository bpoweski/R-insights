#' @import data.table
#' @import jsonlite
#' @import httr

.datatable.aware = TRUE

from_epoch <- function(x) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
}

event_table <- function(x) {
    row <- x

    if (!is.null(x$event)) {
        row <- x$event
    }

    events <- as.data.table(row)
    events[,timestamp := from_epoch(timestamp / 1000)]

    return(events)
}

content_name <- function(x) {
    if (!is.null(x$alias)) {
        return(x$alias)
    } else if (!is.null(x$attribute) && !is.null(x[["function"]])) {
        return(paste(x[["function"]], x$attribute, sep = "."))
    } else if (!is.null(x[["function"]])) {
        return(x[["function"]])
    } else {
        return(NA)
    }
}

facet_table <- function(x) {
    return(do.call(data.table, lapply(x$results, simplify2array)))
}

#' Quotes a character vector
#'
#' @param x A character vector to quote
#' @export
nrql_quote <- function(x) {
    return(paste("'", x, "'", sep=""))
}

is.timeseries <- function(x) {
    return(!is.null(x$metadata$timeSeries))
}

timeseries_table <- function(x) {
    ret <- data.table(begin_time = from_epoch(x$beginTimeSeconds), end_time = from_epoch(x$endTimeSeconds))
    return(cbind(ret, do.call(data.table, lapply(x$results, simplify2array))))
}

#' Parses the New Relic Insights and convert to an R data structure.
#'
#' @param json A character vector containing the JSON to parse.
#' @param include.unknown When \code{TRUE} should `unknownGroup` be included in the result.
#' @return data.table generated from \code{json}
#' @export
parse_insights <- function(json, include.unknown = FALSE) {
    parsed   <- fromJSON(json, simplifyDataFrame = FALSE)
    metadata <- parsed$metadata

    if (!is.null(parsed$error)) {

        stop("received error", parsed$error)

    } else if (length(parsed$results) == 1 && !is.null(parsed$results[[1]]$events)) {

        ## select * from Transaction - style response
        result <- parsed$results[[1]]

        return(rbindlist(lapply(result$events, event_table), fill = TRUE))

    } else if (!is.null(parsed$facets)) {

        facets <- sapply(parsed$facets, function(x) x$name)

        if (is.timeseries(parsed)) {

            ## select f(x) from Transaction facet y timeseries auto - style response
            return(parsed)

        } else {

            ## select f(x) from Transaction facet y - style response
            dt.facet <- data.table(facet = facets)

            if (!is.null(parsed$unknownGroup) && include.unknown) {
                dt.facet <- rbind(dt.facet, data.table(facet = NA))
            }

            setnames(dt.facet, old = "facet", new = metadata$facet)

            ## facet values
            dt.values <- rbindlist(lapply(parsed$facets, facet_table))

            if (!is.null(parsed$unknownGroup) && include.unknown) {
                dt.values <- rbind(dt.values, facet_table(parsed$unknownGroup))
            }

            setnames(dt.values, old = names(dt.values), new = sapply(metadata$contents$contents, content_name))

            dt <- cbind(dt.facet, dt.values)

            return(dt)
        }
    } else if (is.timeseries(parsed)) {

        ret <- rbindlist(lapply(parsed$timeSeries, timeseries_table))

        setnames(ret, old = names(ret)[3:length(ret)], new = sapply(metadata$timeSeries$contents, content_name))

        return(ret)

    } else {

        ## select count(*) from transaction - style response
        ret        <- do.call(c, parsed$results)
        names(ret) <- lapply(parsed$metadata$contents, content_name)

        return(ret)
    }
}

#' Queries New Relic Insights using the character vector nrql.
#'
#' @param nrql A character vector containg the NRQL query.
#' @param account The New Relic insights account.  Uses the environment variable \code{INSIGHTS_ACCOUNT_ID} as a default.
#' @param key The New Relic insights key  Uses the environment variable \code{INSIGHTS_ACCOUNT_KEY} as a default.
#' @param parse If \code{FALSE} returns the unparsed JSON
#' @param ... Passes remaining arguments to \code{\link{parse_insights}}
#' @export
query_insights <- function(nrql, account = Sys.getenv(x = "INSIGHTS_ACCOUNT_ID"), key = Sys.getenv(x = "INSIGHTS_ACCOUNT_KEY"), parse = TRUE, ...) {
    if (is.character(account) && length(account) == 0) {
        stop("No Insights account provided")
    }

    if (length(key) == 0) {
        stop("No Insights key provided")
    }

    url      <- paste0("https://insights-api.newrelic.com/v1/accounts/", account, "/query")
    response <- GET(url = url, query = list(nrql = nrql), add_headers("X-Query-Key" = key, "Accept" = "application/json"))
    json     <- content(response, as = "text")

    if (parse) {
        return(parse_insights(json, ...))
    }

    return(json)
}
