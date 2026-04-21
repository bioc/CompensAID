#' @title Combine segments that do not meet the requirements
#'
#' @param si.input (dataFrame): dataFrame containing SSI info.
#' @param primary (character): Name of the primary marker.
#' @param secondary (character) Name of the secondary marker.
#' @param ev.input (numerical): Minimum required number of events.
#' @param rv.input (numerical): Number of segments.
#'
#' @return (dataFrame) Returns a dataframe with an update SSI information dataFrame.
#' @keywords internal

# Internal function - Combine segments that do not meet the requirements
.UpdateSegments <- function(si.input, primary, secondary, ev.input, rv.input) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertDataFrame(si.input)
  checkmate::assertCharacter(primary)
  checkmate::assertCharacter(secondary)
  checkmate::assertNumeric(ev.input)
  checkmate::assertNumeric(rv.input)
  
  
  # Merge segments -------------------------------------------------------------
  if (si.input$message[si.input$primary.marker == primary &
                       si.input$secondary.marker == secondary][1] != "No positive/negative population") {
    
    # Identify which segments need to be merged
    merge <- .MergeSegments(si.input$event.count[si.input$primary.marker == primary &
                                                  si.input$secondary.marker == secondary],
                           ev.input)
    si.input$mergeGroup <- NA
    
    # Assign merge IDs
    for (i in seq_along(merge)) {
      
      si.input$mergeGroup[si.input$primary.marker == primary &
                            si.input$secondary.marker == secondary][merge[[i]]] <- i
    }
    
    # Adjust segment information
    si.input <- .AdjustSegments(si.input,
                               mp = primary,
                               ms = secondary)
  }
  
  
  # Generate output ------------------------------------------------------------
  return(si.input)
}