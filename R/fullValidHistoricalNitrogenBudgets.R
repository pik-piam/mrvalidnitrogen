#' @title fullValidHistoricalNitrogenBudgets
#' @description creates historical outputs of nitrogen budgets
#' @param aggregate setting for aggregate, e.g.: TRUE, FALSE, "glo"
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidNitrogenPollution}}
#' @examples
#' 
#' \dontrun{ 
#' fullValidHistoricalNitrogenBudgets()
#' }
#' @importFrom  magpiesets reportingnames
#' @importFrom  madrat calcOutput
#' @import mrvalidation
#' @export

fullValidHistoricalNitrogenBudgets<-function(aggregate="glo"){

  NitrogenBudgetCropland <- calcOutput("ValidNitrogenBudgetCropland",aggregate = aggregate)
  NitrogenBudgetCropland[,,"Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter Loss (Mt Nr/yr)"] <- -NitrogenBudgetCropland[,,"Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter Loss (Mt Nr/yr)" ]
  
  demand<-calcOutput("ValidDemand",nutrient="nr",aggregate = aggregate)
  
  production<-calcOutput("ValidProduction",nutrient="nr",aggregate = aggregate)
  
  crop_uptake <- (dimSums(NitrogenBudgetCropland[,,c("Resources|Nitrogen|Cropland Budget|Withdrawals|+|Harvested Crops (Mt Nr/yr)",
                                                     "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Aboveground Crop Residues (Mt Nr/yr)",
                                                     "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Belowground Crop Residues (Mt Nr/yr)")],dim=c(3.3))
                  -dimSums(NitrogenBudgetCropland[,,c("Resources|Nitrogen|Cropland Budget|Inputs|+|Seed (Mt Nr/yr)",
                                                      "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops (Mt Nr/yr)"
                  )],dim=c(3.3)))      
  crop_uptake<-add_dimension(crop_uptake,dim = 3.3,nm = "Resources|Nitrogen|Cropland plant uptake (Mt Nr/yr)")
  trade<-calcOutput("ValidTrade",nutrient="nr", aggregate = aggregate,net_trade=FALSE,equalized=FALSE)
  
  deposition <- calcOutput("ValidNitrogenAtmosphericDeposition",aggregate = aggregate,datasource="Nsurplus2")
  
  x8 <- calcOutput("ValidNitrogenBudgetPlanet",aggregate = aggregate)
  x9 <-  calcOutput("ValidFeed",nutrient="nr",aggregate = aggregate)
  x10 <- calcOutput("ValidManure",aggregate = aggregate)
  x11 <- calcOutput("ValidNitrogenPollution",aggregate = aggregate,datasource="Nsurplus2")
  x12 <- calcOutput("ValidNutrientBudgetLivestock",aggregate = aggregate)
  x13 <- calcOutput("ValidNutrientBudgetFoodWasteAndSewage",aggregate = aggregate)
  x14 <- calcOutput("ValidNutrientBudgetFoodProcessing",aggregate = aggregate,detail=FALSE,nutrient="nr")
  x15 <- calcOutput("ValidNutrientWasteBudget",aggregate =aggregate)
  x16 <- setNames(dimSums(x11[,,c(
    "Emissions|N2O-N|Direct|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|NH3-N|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|NO2-N|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|NO3-N|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|N2-N|Agriculture|+|Animal waste management (Mt Nr/yr)"
  )],dim=3),"Bodirsky.historical.Emissions|N|Agriculture|+|Animal waste management (Mt Nr/yr)")
  x17<-calcOutput("ValidNitrogenBudgetNonagland",aggregate = aggregate)
  x18<-calcOutput("ValidNitrogenBudgetPasture",aggregate = aggregate)
  x19<-calcOutput("ValidNitrogenBudgetOcean",aggregate = aggregate)
  #x20<-calcOutput("ValidNutrientBudgetSewage",nutrient="nr",aggregate = aggregate) # duplicate
  
  out=mbind(NitrogenBudgetCropland,
            demand,production,
            crop_uptake,
            trade,
            deposition,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19)
  
  return(out)
}