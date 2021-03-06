% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtqc.r
\name{rtqc_spike}
\alias{rtqc_spike}
\title{Spike test}
\usage{
rtqc_spike(x, spike.threshold)
}
\arguments{
\item{x}{A vector of values.}

\item{spike.threshold}{A length-2 list specifying "suspect"
and "fail" thresholds. Each threshold can be either a single
value (static threshold) or a vector of same length as \code{x}
(dynamic threshold). Thresholds are typically specific to
location and/or climate and based on expert judgment.}
}
\value{
An ordered factor of test flags of same length as \code{x}.
}
\description{
Perform a spike test. For more information see
U.S. Integrated Ocean Observing System, 2015. Manual for Real-Time
Quality Control of In-situ Temperature and Salinity Data Version
2.0: A Guide to Quality Control and Quality Assurance of In-situ
Temperature and Salinity Observations. 56 pp.
DOI: \href{https://doi.org/10.7289/V5V40SD4}{10.7289/V5V40SD4}.
}
\examples{
fake.data = c(1, 1.1, 1, 1.2, 1, 1.3, 1, 1.4, 1, 1.5, 1)
rtqc_spike(fake.data, c(0.15, 0.25))

}
