% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidNitrogenPollution.R
\name{calcValidNitrogenPollution}
\alias{calcValidNitrogenPollution}
\title{calcValidNitrogenPollution}
\usage{
calcValidNitrogenPollution(datasource = "Nsurplus")
}
\arguments{
\item{datasource}{Bodirsky for own calculations, FAO for some N related parameters published in FAOSTAT.}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Validation Script for Nitrogen Budgets on Croplands
}
\examples{

\dontrun{ 
calcOutput("ValidNitrogenPollution")
}

}
\author{
Benjamin Leon Bodirsky
}
