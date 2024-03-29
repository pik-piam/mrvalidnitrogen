% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidNutrientBudgetLivestock.R
\name{calcValidNutrientBudgetLivestock}
\alias{calcValidNutrientBudgetLivestock}
\title{calcValidNutrientBudgetLivestock}
\usage{
calcValidNutrientBudgetLivestock(datasource = "Bodirsky", nutrient = "nr")
}
\arguments{
\item{datasource}{Bodirsky for own calculations}

\item{nutrient}{The nutrient in which the results shall be reported.}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Validation Script for Nitrogen Budgets for Livestock production
}
\examples{

\dontrun{ 
calcOutput("ValidNutrientBudgetLivestock")
}

}
\author{
Benjamin Leon Bodirsky
}
