#' Calculate Fertilizer of Phosphor
#' 
#' Provides FertP data for Phosphor.No changes to the content have been done.
#' 
#' 
#' @return Fertilizer data for Phosphor and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FertP")
#' 
#' }
#' 
calcFertP <- function() {
  x <- readSource("IFA",subtype="consumption")[,,"Grand Total P2O5", drop=TRUE] * 0.4364/1000
   
  return(list(x=x,
              weight=NULL,
              unit="Tg P",
              description="Fertilizer Phosphor (Grand total P205) from IFA"))
}
