% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcWasteDistrib.R
\name{calcWasteDistrib}
\alias{calcWasteDistrib}
\title{calcWasteDistrib}
\usage{
calcWasteDistrib()
}
\value{
Magpie object of waste types to waste distribution,  percentage
}
\description{
rule-based distribution of waste by composition type to disposal type.
returns list of magpie object, share of total disposal
}
\examples{

\dontrun{ a <- calcOutput(type="WasteDistrib")
}
}
\author{
David Chen
}
