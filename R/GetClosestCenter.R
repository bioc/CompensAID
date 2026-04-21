#' @title Identify value closest to visual estimation
#'
#' @param row (numerical): Numerical values of all density-based cut-off detection values.
#' @param closest (numerical): Numerical value determining the preliminary center.
#'
#' @return (numerical) Returns the value closest to the visual center of the plot.
#' @keywords internal

# Internal function - Identify value closest to visual estimation
.GetClosestCenter <- function(row, closest) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertNumeric(row)
  checkmate::assertNumeric(closest)
  
  
  # Remove NAs from the array --------------------------------------------------
  valid.index <- which(!is.na(row))
  valid.row <- row[valid.index]
  
  
  # Identify the index of the estimations closest to the visual estimation -----
  distances <- abs(row - closest)
  closest <- which.min(distances)
  
  # Get index
  index <- valid.index[closest]
  
  
  # Generate output ------------------------------------------------------------
  return(row[index])
}