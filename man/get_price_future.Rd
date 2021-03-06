% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_price_future.R
\name{get_price_future}
\alias{get_price_future}
\title{Get future price for one day}
\usage{
get_price_future(con, wind_code, beg_dt, end_dt, type, balance, ...)
}
\arguments{
\item{con}{a tiny connection}

\item{wind_code}{vecter of future code list in wind_code format}

\item{beg_dt}{int or or char, format like \%Y\%m\%d, test for this day}

\item{end_dt}{int or or char, format like \%Y\%m\%d, test for this day, default null}

\item{type}{choose price type as close, vwap}

\item{balance}{logi, if a balance panel or not}
}
\value{
if end_date is null
a df with stock code and price
else
a df with stock code and price, trade_dt
}
\description{
Get future price for one day
}
\examples{
\dontrun{
con <- odbcConnect('tiny')
get_price_future(con, 'IF1603.CFE', 20110104, type = 'close')
}

}
