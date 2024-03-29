% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcValidNutrientBudgetFoodProcessing.R
\name{calcValidNutrientBudgetFoodProcessing}
\alias{calcValidNutrientBudgetFoodProcessing}
\title{calcValidNutrientBudgetFoodProcessing}
\usage{
calcValidNutrientBudgetFoodProcessing(
  datasource = "Bodirsky",
  nutrient = "nr",
  detail = FALSE
)
}
\arguments{
\item{datasource}{Bodirsky for own calculations}

\item{nutrient}{The nutrient in which the results shall be reported.}

\item{detail}{shall the funciton reportinghelper provide detailed commodities or only commodity groups?}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Validation Script for Nitrogen Budgets for Food Processing (processing from fooduse to food, not processing from one product to another)
}
\examples{

\dontrun{ 
calcOutput("ValidNutrientBudgetFoodProcessing")
}

}
\author{
Benjamin Leon Bodirsky
}
