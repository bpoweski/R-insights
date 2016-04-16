library(data.table)
library(jsonlite)
library(httr)

.datatable.aware = TRUE

as.event.table <- function(x) {
    events <- as.data.table(x)
    events[,timestamp := as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")]

    return(events)
}

content.name <- function(x) {
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

as.facet.table <- function(x) {
    return(do.call(data.table, x$results))
}

#' @export
parse.insights <- function(json) {
    parsed <- fromJSON(json, simplifyDataFrame=FALSE)

    if (length(parsed$results) == 1 && !is.null(parsed$results[[1]]$events)) {

        result <- parsed$results[[1]]

        return(rbindlist(lapply(result$events, function(x) as.event.table(x$event)), fill=TRUE))

    } else if (!is.null(parsed$facets)) {

        ## determine facet names
        dt.facet <- data.table(facet=sapply(parsed$facets, function(x) x$name))

        if (!is.null(parsed$unknownGroup)) {
            dt.facet <- rbind(dt.facet, data.table(facet=NA))
        }

        setnames(dt.facet, old="facet", new=parsed$metadata$facet)

        ## facet values
        dt.values <- rbindlist(lapply(parsed$facets, as.facet.table))

        if (!is.null(parsed$unknownGroup)) {
            dt.values <- rbind(dt.values, as.facet.table(parsed$unknownGroup))
        }

        setnames(dt.values, old=names(dt.values), new=sapply(parsed$metadata$contents$contents, content.name))

        dt <- cbind(dt.facet, dt.values)

        return(dt)

    } else if (is.null(parsed$error)) {

        ret        <- do.call(c, parsed$results)
        names(ret) <- lapply(parsed$metadata$contents, content.name)

        return(ret)

    } else {

        stop("received error", parsed$error)

    }
}

#' @export
query.insights <- function(nrql, account=Sys.getenv(x="INSIGHTS_ACCOUNT_ID"), key=Sys.getenv(x="INSIGHTS_ACCOUNT_KEY"), parse=TRUE) {
    if (length(account) == 0) {
        stop("No Insights account provided")
    }

    if (length(key) == 0) {
        stop("No Insights key provided")
    }

    url      <- paste0("https://insights-api.newrelic.com/v1/accounts/", account, "/query")
    response <- GET(url = url, query = list(nrql = nrql), add_headers("X-Query-Key" = key, "Accept" = "application/json"))
    json     <- content(response, as = "text")

    if (parse) {
        return(parse.insights(json))
    }

    return(json)
}
