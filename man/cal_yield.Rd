% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_yield.R
\name{cal_yield}
\alias{cal_yield}
\title{calculate the yield}
\usage{
cal_yield(x, first = 0)
}
\arguments{
\item{x}{vector of net value}

\item{fisrt, }{the first day yield}
}
\description{
calculate the yield from net value
}
\examples{
data(m_index_acount)
cal_yield(m_index_acount$acount)

}
