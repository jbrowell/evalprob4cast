% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_marginal_distribution.R
\name{evaluate_marginal_distribution}
\alias{evaluate_marginal_distribution}
\title{Forecast evaluation}
\usage{
evaluate_marginal_distribution(data, by_lead_time = T, metric = "CRPS")
}
\arguments{
\item{data}{A list containing forecasts and observations.}

\item{by_lead_time}{Logical indicator for whether forecast evaluation should be lead time-specific or not. Defaults to true.}

\item{metric}{The chosen metric/scoring rule. Available options are "CRPS" (default) and "LogS". LogS should never be used on datasets where some forecasts only have one ensemble member.}
}
\value{
A table of scores for the dataset.
}
\description{
Forecast evaluation
}
