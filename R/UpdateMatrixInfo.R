#' @title Calculate segment information
#'
#' @param si.input (dataFrame): dataFrame containing SSI info.
#' @param range.input (numerical): Width of each segment.
#' @param primary (character): Name of the primary marker.
#' @param secondary (character) Name of the secondary marker.
#' @param output (character): Will be "PASS" or "No positive/negative population" depending on the required events.
#' @param rv.input (numerical): Number of segments.
#' @param population (matrix): Matrix with the population for which the information is obtained.
#'
#' @return (dataFrame) Returns a dataframe with an update SSI information dataFrame.
#' @keywords internal

# Internal function - Calculate segment information
.UpdateMatrixInfo <- function(si.input, range.input = NULL, primary, secondary, output, rv.input, population = NULL) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertDataFrame(si.input)
  checkmate::assertCharacter(primary)
  checkmate::assertCharacter(secondary)
  checkmate::assertCharacter(output)
  checkmate::assertNumeric(rv.input)
  
  aditVal <- !is.null(range.input)
  if(aditVal) {
  checkmate::assertNumeric(range.input)
  checkmate::assertList(population)
  }
  
  # Channel names
  channel.primary <- si.input$primary.channel[si.input$primary.marker == primary & si.input$secondary.marker == secondary][1]
  channel.secondary <- si.input$secondary.channel[si.input$primary.marker == primary & si.input$secondary.marker == secondary][1]
  
  
  # Update SSI information if there is no positive/negative population ---------
  if (output == "No positive/negative population") {
    col.adjust <- c("segment.min", "segment.max", "event.count", "event.count.merge", "mfi.neg", "sd.neg", "mfi.pos", "ssi")
    si.input[si.input$primary.marker == primary & si.input$secondary.marker == secondary, col.adjust] <- NA
    si.input[si.input$primary.marker == primary & si.input$secondary.marker == secondary, "message"] <- "No positive/negative population"
  }
  
  
  # Update SSI information if there is a positive/negative population ----------
  if (output == "PASS") {
    
    # Information for the positive population
    si.input[si.input$primary.marker == primary & si.input$secondary.marker == secondary, "message"] <- "PASS"
    population.positive <- population$primary.positive
    
    
    # Obtain information per segment -------------------------------------------
    for (s in seq_len(rv.input)) {
      
      # Minimum fluorescence value within the segment
      si.input$segment.min[si.input$primary.marker == primary &
                             si.input$secondary.marker == secondary &
                             si.input$segment == s] <- .GetSegment(population = population.positive,
                                                                  primary.channel = channel.primary,
                                                                  secondary.channel = channel.secondary,
                                                                  segment = s,
                                                                  range.input = range.input)[,"min"]
      
      # Maximum fluorescence value within the segment
      si.input$segment.max[si.input$primary.marker == primary &
                             si.input$secondary.marker == secondary &
                             si.input$segment == s] <- .GetSegment(population = population.positive,
                                                                  primary.channel = channel.primary,
                                                                  secondary.channel = channel.secondary,
                                                                  segment = s,
                                                                  range.input = range.input)[,"max"]
      
      # Number of events within the segment
      si.input$event.count[si.input$primary.marker == primary &
                             si.input$secondary.marker == secondary &
                             si.input$segment == s] <- .GetSegment(population = population.positive,
                                                                  primary.channel = channel.primary,
                                                                  secondary.channel = channel.secondary,
                                                                  segment = s,
                                                                  range.input = range.input)[,"count"]
      
      # Mean fluorescence intensity of the segment
      si.input$mfi.pos[si.input$primary.marker == primary &
                         si.input$secondary.marker == secondary &
                         si.input$segment == s] <- .GetSegment(population = population.positive,
                                                              primary.channel = channel.primary,
                                                              secondary.channel = channel.secondary,
                                                              segment = s,
                                                              range.input = range.input)[,"mfi.pos"]
    }
    
    
    # Obtain information for the negative population ---------------------------
    population.negative <- population$primary.negative
    
    # Mean fluorescence intensity of the negative population
    si.input$mfi.neg[si.input$primary.marker == primary &
                       si.input$secondary.marker == secondary] <- stats::median(population.negative[, channel.secondary])
    
    # Standard deviation of the negative population
    si.input$sd.neg[si.input$primary.marker == primary &
                      si.input$secondary.marker == secondary] <- stats::sd(population.negative[, channel.secondary])
    
    
    # Calculate the Secondary Stain Index --------------------------------------
    for (s in seq_len(rv.input)) {
      
      # SSI per segment
      si.input$ssi[si.input$primary.marker == primary &
                     si.input$secondary.marker == secondary &
                     si.input$segment == s] <- .CalculateSSI(si.input = si.input,
                                                            primary.channel = channel.primary,
                                                            secondary.channel = channel.secondary,
                                                            segment = s)
    }
  }
  
  
  # Generate output ------------------------------------------------------------
  return(si.input)
}
