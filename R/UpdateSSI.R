#' @title Update Secondary Stain Index
#'
#' @param si.input (dataFrame): DataFrame containing the SSI info.
#' @param rv.input (numerical): Numerical value for the number of segments.
#' @param primary (character): Name of the primary marker
#' @param secondary (character): Name of the secondary marker
#' @param population (list): List with the population matrices
#'
#' @return (numerical) Returns the Secondary Stain Index score.
#' @keywords internal

# Internal function - Update Secondary Stain Index
.UpdateSSI <- function(si.input, rv.input, primary, secondary, population) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertDataFrame(si.input)
  checkmate::assertNumeric(rv.input)
  checkmate::assertCharacter(primary)
  checkmate::assertCharacter(secondary)
  checkmate::assertList(population)
  
  # Obtain information
  population.positive <- population$primary.positive
  primary.channel <- si.input$primary.channel[si.input$primary.marker == primary & si.input$secondary.marker == secondary][1]
  secondary.channel <- si.input$secondary.channel[si.input$primary.marker == primary & si.input$secondary.marker == secondary][1]
  
  # Update SSI per segment -----------------------------------------------------
  for (s in seq_len(rv.input)) {
    
    # Skip iteration if segment is ok
    if (si.input$message[si.input$primary.channel == primary.channel &
                         si.input$secondary.channel == secondary.channel &
                         si.input$segment == s] == "PASS" |
        is.na(si.input$message[si.input$primary.channel == primary.channel &
                               si.input$secondary.channel == secondary.channel &
                               si.input$segment == s])) { next }
    
    # Get segment minimum
    new.min <- si.input$segment.min[si.input$primary.channel == primary.channel &
                                      si.input$secondary.channel == secondary.channel &
                                      si.input$segment == s]
    
    # Get segment maximum
    new.max <- si.input$segment.max[si.input$primary.channel == primary.channel &
                                      si.input$secondary.channel == secondary.channel &
                                      si.input$segment == s]
    
    
    # Obtain new segments ------------------------------------------------------
    positive.sub <- population.positive[population.positive[, primary.channel] >= new.min & population.positive[, primary.channel] <= new.max, , drop = FALSE]
    
    # Get mean fluorescence intensity value
    si.input$mfi.pos[si.input$primary.channel == primary.channel &
                       si.input$secondary.channel == secondary.channel &
                       si.input$segment == s] <- stats::median(positive.sub[, secondary.channel])
    
    # Get secondary stain index
    si.input$ssi[si.input$primary.channel == primary.channel &
                   si.input$secondary.channel == secondary.channel &
                   si.input$segment == s] <- .CalculateSSI(si.input = si.input,
                                                          primary.channel = primary.channel,
                                                          secondary.channel = secondary.channel,
                                                          segment = s)
  }
  
  
  # Generate output ------------------------------------------------------------
  return(si.input)
}