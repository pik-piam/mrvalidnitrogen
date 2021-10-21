#' @title fullVALIDHISTORICALNITROGENBUDGETS
#' @description creates historical outputs of nitrogen budgets
#' @param aggregate setting for aggregate, e.g.: TRUE, FALSE, "glo"
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcValidNitrogenPollution}}
#' @examples
#' \dontrun{
#' fullVALIDHISTORICALNITROGENBUDGETS()
#' }
#' @importFrom  magpiesets reportingnames
#' @importFrom  madrat calcOutput
#' @import mrvalidation
#' @export
fullVALIDHISTORICALNITROGENBUDGETS <- function(aggregate = "glo") {
  NitrogenBudgetCropland <- calcOutput("ValidNitrogenBudgetCropland", aggregate = aggregate)
  NitrogenBudgetCropland[, , "Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter Loss (Mt Nr/yr)"] <- -NitrogenBudgetCropland[, , "Resources|Nitrogen|Cropland Budget|Balance|+|Soil Organic Matter Loss (Mt Nr/yr)"]

  demand <- calcOutput("ValidDemand", nutrient = "nr", aggregate = aggregate)

  production <- calcOutput("ValidProduction", nutrient = "nr", aggregate = aggregate)

  crop_uptake <- (dimSums(NitrogenBudgetCropland[, , c(
    "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Harvested Crops (Mt Nr/yr)",
    "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Aboveground Crop Residues (Mt Nr/yr)",
    "Resources|Nitrogen|Cropland Budget|Withdrawals|+|Belowground Crop Residues (Mt Nr/yr)"
  )], dim = c(3.3))
  - dimSums(NitrogenBudgetCropland[, , c(
      "Resources|Nitrogen|Cropland Budget|Inputs|+|Seed (Mt Nr/yr)",
      "Resources|Nitrogen|Cropland Budget|Inputs|+|Biological Fixation Symbiotic Crops (Mt Nr/yr)"
    )], dim = c(3.3)))
  crop_uptake <- add_dimension(crop_uptake, dim = 3.3, nm = "Resources|Nitrogen|Cropland plant uptake (Mt Nr/yr)")
  trade <- calcOutput("ValidTrade", nutrient = "nr", aggregate = aggregate, net_trade = FALSE, equalized = FALSE)

  deposition <- calcOutput("ValidNitrogenAtmosphericDeposition", aggregate = aggregate, datasource = "Nsurplus2")

  x8 <- calcOutput("ValidNitrogenBudgetPlanet", aggregate = aggregate)
  x9 <- calcOutput("ValidFeed", nutrient = "nr", aggregate = aggregate)
  x10 <- calcOutput("ValidManure", aggregate = aggregate)
  x11 <- calcOutput("ValidNitrogenPollution", aggregate = aggregate, datasource = "Nsurplus2")
  x12 <- calcOutput("ValidNutrientBudgetLivestock", aggregate = aggregate)
  x13 <- calcOutput("ValidNutrientBudgetFoodWasteAndSewage", aggregate = aggregate)
  x14 <- calcOutput("ValidNutrientBudgetFoodProcessing", aggregate = aggregate, detail = FALSE, nutrient = "nr")
  x15 <- calcOutput("ValidNutrientWasteBudget", aggregate = aggregate)
  x16 <- setNames(dimSums(x11[, , c(
    "Emissions|N2O-N|Direct|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|NH3-N|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|NO2-N|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|NO3-N|Agriculture|+|Animal waste management (Mt Nr/yr)",
    "Emissions|N2-N|Agriculture|+|Animal waste management (Mt Nr/yr)"
  )], dim = 3), "Bodirsky.historical.Emissions|N|Agriculture|+|Animal waste management (Mt Nr/yr)")
  x17 <- calcOutput("ValidNitrogenBudgetNonagland", aggregate = aggregate)
  x18 <- calcOutput("ValidNitrogenBudgetPasture", aggregate = aggregate)
  x19 <- calcOutput("ValidNitrogenBudgetOcean", aggregate = aggregate)
  # x20<-calcOutput("ValidNutrientBudgetSewage",nutrient="nr",aggregate = aggregate) # duplicate

  crop_bnf_detail <- calcOutput("NitrogenFixationPast", fixation_types = "fixation_crops", sum_plantparts = TRUE, cellular = FALSE, irrigation = FALSE, aggregate = aggregate)
  crop_bnf_forage <- setNames(crop_bnf_detail[, , "foddr"], paste0("Resources|Nitrogen|Cropland Budget|Inputs|Biological Fixation Symbiotic Crops|+|", reportingnames("foddr"), " (Mt Nr/yr)"))
  crop_bnf_crops <- setNames(dimSums(crop_bnf_detail[, , "foddr", invert = TRUE], dim = "ItemCodeItem"), paste0("Resources|Nitrogen|Cropland Budget|Inputs|Biological Fixation Symbiotic Crops|+|Crops (Mt Nr/yr)"))
  res_ag <- collapseNames(calcOutput("ResBiomass", cellular = FALSE, plantparts = "ag", irrigation = FALSE, attributes = "nr", aggregate = aggregate))[,getYears(crop_bnf_crops),]
  res_bg <- collapseNames(calcOutput("ResBiomass", cellular = FALSE, plantparts = "bg", irrigation = FALSE, attributes = "nr", aggregate = aggregate))[,getYears(crop_bnf_crops),]
  res_ag_forage <- setNames(res_ag[, , "foddr"], paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|Aboveground Crop Residues|+|", reportingnames("foddr"), " (Mt Nr/yr)"))
  res_ag_crops <- setNames(dimSums(res_ag[, , "foddr", invert = TRUE], dim = "ItemCodeItem"), paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|Aboveground Crop Residues|+|Crops (Mt Nr/yr)"))
  res_bg_forage <- setNames(res_bg[, , "foddr"], paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|Belowground Crop Residues|+|", reportingnames("foddr"), " (Mt Nr/yr)"))
  res_bg_crops <- setNames(dimSums(res_bg[, , "foddr", invert = TRUE], dim = "ItemCodeItem"), paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|Belowground Crop Residues|+|Crops (Mt Nr/yr)"))
  
  foragesplit <- mbind(
    crop_bnf_forage, crop_bnf_crops,
    res_ag_forage, res_ag_crops,
    res_bg_forage, res_bg_crops
  )
  foragesplit <- add_dimension(foragesplit, dim = 3.1, add = "scenario", nm = "historical")
  foragesplit <- add_dimension(foragesplit, dim = 3.1, add = "model", nm = "Nsurplus2")

  # calculate recycling and burning by crops and forage

    past              <- findset("past")
    relevant_nutrients <- c("nr")
    
    res_biomass        <- collapseNames(calcOutput("ResBiomass", cellular=F, plantparts="ag", aggregate = FALSE, scenario="default"))[,,relevant_nutrients][,past,]
    
    burnshr           <- calcOutput("ResCombustEff",aggregate = FALSE)[,,getNames(res_biomass,dim=1)]
    dev_state_past    <- collapseNames(calcOutput("DevelopmentState",aggregate = F)[,past,"SSP2"])
    burn    <-  ash               <- res_biomass * (dev_state_past*0.15+(1-dev_state_past)*0.25)
    ash[,,c("nr")] <- ash[,,c("nr")]*(1-burnshr)  ## assuming the same for C and Nr, maybe has to be updated
    burn                          <- burn - ash  
    # assume that forage residues are only burned, but not harvested
    production_forage = dimSums(res_biomass[,,"foddr"],dim = c(1,3))
    getItems(production_forage,dim = 1) <- "GLO"
    burn_forage = setNames(dimSums(burn[,,"foddr"],dim = c(1,3)), "Resources|Nitrogen|Residue Burning Budget|Inputs|Burned Residues|+|Forages (Mt Nr/yr)")
    burn_crop = setNames(dimSums(burn[,,"foddr",invert = T],dim=c(1,3)),"Resources|Nitrogen|Residue Burning Budget|Inputs|Burned Residues|+|Crops (Mt Nr/yr)")
    getItems(burn_forage,dim = 1) <- "GLO"
    getItems(burn_crop,dim = 1) <- "GLO"
    
    recycled_forage =  setNames(production_forage - burn_forage,"Resources|Nitrogen|Cropland Budget|Inputs|Recycled Aboveground Crop Residues|+|Forages (Mt Nr/yr)")
    recycled_crop = setNames(
      NitrogenBudgetCropland[,,"Resources|Nitrogen|Cropland Budget|Inputs|+|Recycled Aboveground Crop Residues (Mt Nr/yr)"] 
      - recycled_forage,"Resources|Nitrogen|Cropland Budget|Inputs|Recycled Aboveground Crop Residues|+|Crops (Mt Nr/yr)")
    
    res_detailed=mbind(
      burn_forage, 
      burn_crop,
      recycled_forage,
      recycled_crop)
    res_detailed=add_dimension(x=res_detailed,dim = 3.1,add = "scenario",nm = "history")
    res_detailed=add_dimension(x=res_detailed,dim = 3.1,add = "model",nm = "MADRAT")
    
  
  forestry_products=function(){
    past              <- findset("past")
    x <- readSource("FAO_online","ForestProdTrade")
    
    ## Remove distinction between coniferous and non-coniferous part
    x <- x[,,sort(getNames(x)[grep(pattern = "coniferous",x = getNames(x),invert = T)])]
    
    ## Remove strange numbers (FAO item codes?)
    getNames(x,dim = "ItemCodeItem") <- gsub(pattern = "^[0-9]+\\|",
                                             replacement = "",
                                             x = getNames(x,dim = "ItemCodeItem"))
    
    ## Remove monetary data, we are only interested in m3 or mio. ton data
    x <- x[,,grep(pattern = "kUS\\$", x = getNames(x,dim = "ElementShort"), invert = T, value = T)]
    x <- x[,,sort(getNames(x))]
    
    ## FAO separated "Particle board and OSB" into "Particle board" and "Particle board and OSB (1961-1994)"
    ## Why? Who knows
    ## Merging back into "Particle board and OSB"
    x[,,"Particle board and OSB (1961-1994)"] <- x[,,"Particle board and OSB (1961-1994)"] + x[,,"Particle board"]
    
    ## drop "Particle Board"
    x <- x[,,"Particle board",invert=TRUE]
    
    ## Rename "Particle board and OSB (1961-1994)" to "Particle board and OSB"
    getNames(x,dim="ItemCodeItem") <- gsub(pattern = "Particle board and OSB \\(1961-1994\\)",replacement = "Particle board and OSB",x = getNames(x,dim="ItemCodeItem"))
    
    ## Pulpwood category is strangely split in data because it'll be a miracle if FAO could stick to consistent naming
    ## we now merge this in one
    pulpwood_names <- grep(pattern = "Pulpwood",x = getNames(x,dim=1),value = TRUE)
    
    ## isolate all pulpwood data
    pulpwood_data <- x[,,pulpwood_names]
    
    ## Repitition of export, import data and unnecessary split has been made in three categories
    ## "Pulpwood and particles (1961-1997).export_m3" and "Pulpwood, round and split, all species (export/import, 1961-1989).export_m3"
    ## report the same data. We can just use "Pulpwood and particles (1961-1997).export_m3"
    pulpwood_export <- pulpwood_data[,,"Pulpwood and particles (1961-1997).export_m3"]
    pulpwood_import <- pulpwood_data[,,"Pulpwood and particles (1961-1997).import_m3"]
    ## Merge production data
    pulpwood_production <- pulpwood_data[,,"Pulpwood and particles (1961-1997).production"] + setNames(pulpwood_data[,,"Pulpwood, round and split, all species (production).production"],NULL)
    
    ## Corrected pulpwood data
    pulpwood <- mbind(pulpwood_production,pulpwood_export,pulpwood_import)
    getNames(pulpwood,dim=1) <- "Pulpwood"
    
    ## Pulpwood export and import data is not available after 1989 (from 1990)
    last_share_export <- collapseNames(pulpwood[,1989,"export_m3"]/pulpwood[,1989,"production"])
    last_share_export[is.na(last_share_export)] <- 0
    last_share_export[last_share_export>1] <- 1 ## COG exports more than production??
    
    last_share_import <- collapseNames(pulpwood[,1989,"import_m3"]/pulpwood[,1989,"production"])
    last_share_import[is.na(last_share_import)] <- 0
    last_share_import[last_share_import>1] <- 1 
    
    pulpwood[,1990:2019,"export_m3"] <- pulpwood[,1990:2019,"production"] * last_share_export
    pulpwood[,1990:2019,"import_m3"] <- pulpwood[,1990:2019,"production"] * last_share_import
    ## Pulpwood export import data is not reported 
    
    ## remove pulpwood data fom raw data momentarily
    x <- x[,,pulpwood_names,invert=TRUE]
    
    ## Add back pulpwood
    x <- mbind(x,pulpwood)
    
    # Sort naming
    x <- x[,,sort(getNames(x,dim=1))]
    
    ## Extract variables we need and only choose variables which which have data for all three categories:
    ## Production in m3
    ## Import in m3
    ## Export in m3
    ## Here, wood pulp is an exception which is in mio. tonnes so we will assume 450 kg/m3 density there
    
    #paper and cardboard N content: 0.225% (paper from domestic refuse)
    # wood etc using 0.09%, lower than roundwood because of missing bark, average of softwoods and ahrdwoos
    # based on Rynk, Robert. n.d. ‘On-Farm Composting Handbook’, 204.
    
    # 0.6 is * Volumetric m3 to ton conversion
    #* 0.6 ton DM / m^3
    #* Conversion factor of roundwood  : 632.5 kg/m3 (mean value) as in FAO Document (http://www.fao.org/3/a-i4441e.pdf), Page 6, table 4.
    #* Conversion factor of wood fuel  : 307.1 kg/m3 (mean value) as in FAO Document (http://www.fao.org/3/a-i4441e.pdf), Page 7, table 6.
    
    
    paper= x[,,"Paper and paperboard"][,,"production"] * 0.00225
    rest = x[,,c("Other industrial roundwood", "Sawnwood","Wood-based panels")][,,"production"] * 0.0009 * 0.6 
    out=mbind(paper, rest)/1000000
    out<-collapseNames(out)[,past,]
    
    forest_prod <- calcOutput("FAOmassbalance",aggregate = F)[,,"dm"][,,"production"][,,c("wood","woodfuel")]*0.00159
    forest_prod <- collapseNames(forest_prod)
    getItems(forest_prod,dim = 3.1) = paste0(getItems(forest_prod,dim = 3.1)," (Mt Nr/yr)")
    out<-mbind(out,forest_prod)
    
    ### validation_format
    
    
    
    out<-add_dimension(x=out,dim = 3.1,add = "scenario",nm = "history")
    out<-add_dimension(x=out,dim = 3.1,add = "model",nm = "MADRAT")
    
    return(out)
  }
  
  forestry <- forestry_products()[,past,]
  forestry <- dimSums(forestry,dim=1)
  getItems(forestry,dim=1) = "GLO"
  
  out <- mbind(
    NitrogenBudgetCropland,
    demand, production,
    crop_uptake,
    trade,
    deposition, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19,
    foragesplit,
    res_detailed,
    forestry
  )

  return(out)
}
