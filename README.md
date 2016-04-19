# R-insights

[![Build Status](https://travis-ci.org/bpoweski/R-insights.png?branch=master)](https://travis-ci.org/bpoweski/R-insights)

R-insights is a package to query New Relic Insights into R friendly data structures.


### Development Installation

```R
install.packages("devtools")
devtools::install_github("bpoweski/R-insights")
```

### Usage

First, you need to create a query key in [New Relic Insights](https://insights.newrelic.com).

Next you'll need to find your Account ID which is visible on the create key screen and also part of the URL for your new relic account.

```
https://insights.newrelic.com/accounts/<account-id>/query
```

To query insights, simply pass the query and relevant credentials the the `query_insights` function.

``` R
> library(insights)
> dt <- query_insights(nrql = "SELECT count(*) FROM PageView FACET userAgentName", account = <account-id>, key = <insights-key>)
> dt
    userAgentName count
1:         Chrome  4306
2:             IE   917
3:        Firefox   910
4:         Safari   416
5: Microsoft Edge    60
6:      IE Mobile     1
```

In order to not have to pass these values over and over, `account` and `key` default to the environment variables `INSIGHTS_ACCOUNT_ID` and `INSIGHTS_ACCOUNT_KEY`.

``` R
> query_insights("SELECT count(*) FROM PageView FACET userAgentOS")
        userAgentOS count
1:          Windows  6003
2:              Mac   377
3:             iPad   189
4:            Linux   149
5:           iPhone    95
6:          Android    62
7: Google Chrome OS    10
8:   Windows Mobile     1
```

Events and facets are returned as `data.table` objects.

```R
> query_insights("SELECT webAppDuration FROM PageView LIMIT 1")
             timestamp webAppDuration
1: 2016-04-19 16:10:23          4.989
```

Aliases are used as table columns.  This is particularly useful for generating plots from the data.

```R
> library(ggplot2)
> dt <- query_insights("SELECT count(*) as 'pageViews' FROM PageView FACET userAgentOS")
> dt
        userAgentOS pageViews
1:          Windows      6172
2:              Mac       321
3:             iPad       158
4:            Linux       156
5:           iPhone       124
6:          Android        60
7: Google Chrome OS        12
> ggplot(dt) + geom_bar(aes(x = userAgentOS, y = pageViews), stat = "identity")
```


Timestamps are converted into `POSIXct` objects.

``` R
> class(query_insights("SELECT webAppDuration FROM PageView LIMIT 1")$timestamp)
[1] "POSIXct" "POSIXt"
```

Aggregations are returned as lists.

``` R
> query_insights("SELECT count(*) FROM PageView WHERE city = 'Dallas'")
$count
[1] 882
```
