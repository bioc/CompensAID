#' @title Detect if smaller peaks inclusion improved the density-based cut-off detection.
#'
#' @param old.limit (numerical): Numerical value of the limit detected under default settings.
#' @param new.limit (numerical): Numerical value of the limit detected under adjusted settings
#' @param center.plot (numerical): Numerical value of the estimated center.
#'
#' @return (numerical) Returns the limit that fits the data best. 
#' @keywords internal

# Internal function - Detect if smaller peaks inclusion improved the density-based cut-off detection.
.GetClosestLimit <- function(old.limit, new.limit, center.plot) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertNumeric(old.limit, len = 1)
  checkmate::assertNumeric(new.limit, len = 1)
  checkmate::assertNumeric(center.plot, len = 1)
  
  # Identify distance from center.plot
  distance.old <- abs(old.limit - center.plot)
  distance.new <- abs(new.limit - center.plot)
  
  # Determine closest
  if (distance.old < distance.new) {
    best.limit <- old.limit
  } else {
    best.limit <- new.limit 
  }
  
  
  # Generate output ------------------------------------------------------------
  return(best.limit)
}