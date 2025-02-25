#' Sample data included in package
#' 
#' Wind speed ensemble forecasts from the MSEPS Ensemble with 75 members/forecasts
#' and observations from the corresponding Synop Station in Denmark called Thorø.
#'
#' @name IEAW51-MSEPS-DKsynop48
#' @docType data
#' @keywords data
#' @format list with two entries, \code{observations} and \code{forecasts}. 
#' \code{forecasts} is a list of \code{data.frame}s containing forecast data. 
"mseps_dk_thoroe48"

#' Regional capacity factor estimates and forecast
#'
#' Dummy data for the purpose of testing this package. Hourly capacity factor
#' estimated using hourly (spatial) mean wind speed from ERA5 for a region in
#' the UK and forecasts of the same for three regions from ECMWF's 50 member
#' ensemble extracted from the TIGGE archive.
#' 
#' Data derived form ERA5 were Generated using Copernicus Climate Change Service
#' information [2023]. Data derived from ECMWF are governed by the (Creative
#' Commons Attribution 4.0 International
#' (CC BY 4.0))[https://creativecommons.org/licenses/by/4.0/].
#'
#' @name ecmwf_sample
#' @docType data
#' @keywords data
#' @format list with two entries, \code{observations} and \code{forecasts}. 
#' \code{forecasts} is a list of \code{data.frame}s containing forecast data. 
"fc_obs_data"