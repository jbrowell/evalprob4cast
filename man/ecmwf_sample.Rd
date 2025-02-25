% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ecmwf_sample}
\alias{ecmwf_sample}
\alias{fc_obs_data}
\title{Regional capacity factor estimates and forecast}
\format{
list with two entries, \code{observations} and \code{forecasts}. 
\code{forecasts} is a list of \code{data.frame}s containing forecast data.
}
\usage{
fc_obs_data
}
\description{
Dummy data for the purpose of testing this package. Hourly capacity factor
estimated using hourly (spatial) mean wind speed from ERA5 for a region in
the UK and forecasts of the same for three regions from ECMWF's 50 member
ensemble extracted from the TIGGE archive.
}
\details{
Data derived form ERA5 were Generated using Copernicus Climate Change Service
information [2023]. Data derived from ECMWF are governed by the (Creative
Commons Attribution 4.0 International
(CC BY 4.0))[https://creativecommons.org/licenses/by/4.0/].
}
\keyword{data}
