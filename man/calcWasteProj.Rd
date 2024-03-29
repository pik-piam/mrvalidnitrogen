% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcWasteProj.R
\name{calcWasteProj}
\alias{calcWasteProj}
\title{calcWasteProj}
\usage{
calcWasteProj(pc = TRUE, SSP = "SSP2")
}
\arguments{
\item{pc}{per capita (kg/capita) or total (Mt)}

\item{SSP}{SSP scenario}
}
\value{
magpie object of waste projections by treatment and type 
#' @importFrom tidyverse spread select inner_join filter
#' @importFrom tidyr unite
#' @importFrom DirichletReg DR_data DirichReg predict
}
\description{
Calculates all waste projections, multiplies shares properly by pc or total generation quantities
}
\author{
David Chen
}
