#' @title Assess event requirement
#'
#' @param negative (matrix): All events that fall within the negative primary population.
#' @param positive (matrix): All events that fall within the positive primary population.
#' @param events.value (numerical): Numerical value defining the minimum requirement of events.
#'
#' @return (boolean) Returns TRUE/FALSE depending on the required number of events.
#' @keywords internal

# Internal function - Assess event requirement
.EventRequirement <- function(negative, positive, events.value) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertMatrix(negative)
  checkmate::assertMatrix(positive)
  checkmate::assertNumeric(events.value)
  
  # Obtain numer of events within each population
  eventPos <- nrow(positive)
  eventNeg <- nrow(negative)
  
  
  # Assess event requirement ---------------------------------------------------
  outcome <- ifelse(eventPos < events.value | eventNeg < events.value, TRUE, FALSE)
  
  
  # Generate output ------------------------------------------------------------
  return(outcome)
}
