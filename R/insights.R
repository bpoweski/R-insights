#' @import data.table
#' @import jsonlite
#' @import httr

.datatable.aware = TRUE

event_table <- function(x) {
    events <- as.data.table(x)
    events[,timestamp := as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")]

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

#' Parses the New Relic Insights and convert to an R data structure.
#'
#' @param json A character vector containing the JSON to parse.
#' @param include.unknown When \code{TRUE} should `unknownGroup` be included in the result.
#' @return data.table generated from \code{json}
#' @export
parse_insights <- function(json, include.unknown = FALSE) {
    parsed <- fromJSON(json, simplifyDataFrame=FALSE)

    if (length(parsed$results) == 1 && !is.null(parsed$results[[1]]$events)) {

        result <- parsed$results[[1]]

        return(rbindlist(lapply(result$events, function(x) event_table(x$event)), fill=TRUE))

    } else if (!is.null(parsed$facets)) {

        ## determine facet names
        dt.facet <- data.table(facet=sapply(parsed$facets, function(x) x$name))

        if (!is.null(parsed$unknownGroup) && include.unknown) {
            dt.facet <- rbind(dt.facet, data.table(facet=NA))
        }

        setnames(dt.facet, old="facet", new=parsed$metadata$facet)

        ## facet values
        dt.values <- rbindlist(lapply(parsed$facets, facet_table))

        if (!is.null(parsed$unknownGroup) && include.unknown) {
            dt.values <- rbind(dt.values, facet_table(parsed$unknownGroup))
        }

        setnames(dt.values, old=names(dt.values), new=sapply(parsed$metadata$contents$contents, content_name))

        dt <- cbind(dt.facet, dt.values)

        return(dt)

    } else if (is.null(parsed$error)) {

        ret        <- do.call(c, parsed$results)
        names(ret) <- lapply(parsed$metadata$contents, content_name)

        return(ret)

    } else {

        stop("received error", parsed$error)

    }
}


#' Queries New Relic Insights using the character vector nrql.
#'
#' @param nrql A character vector containg the NRQL query.
#' @param account The New Relic insights account.  Uses the environment variable \code{INSIGHTS_ACCOUNT_ID} as a default.
#' @param key The New Relic insights key  Uses the environment variable \code{INSIGHTS_ACCOUNT_KEY} as a default.
#' @param parse If \code{FALSE} returns the unparsed JSON
#' @param ... Passes remaining arguments to \code{parse_insights}
#' @export
query_insights <- function(nrql, account=Sys.getenv(x="INSIGHTS_ACCOUNT_ID"), key=Sys.getenv(x="INSIGHTS_ACCOUNT_KEY"), parse=TRUE, ...) {
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
