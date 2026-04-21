#' @title Calculate the Secondary Stain Index for each marker combination
#'
#' @param si.input (dataFrame): DataFrame containing the SSI info.
#' @param primary.channel (character): Name of the primary marker.
#' @param secondary.channel (character): Name of the secondary marker.
#' @param segment (numeric): Numerical value of the segment
#' 
#' @return Numeric value of Secondary Stain Index score
#' @keywords internal

# Internal function - Calculate Secondary Stain Index
.CalculateSSI <- function(si.input, primary.channel, secondary.channel, segment) {
  
  
  # Input validation -----------------------------------------------------------
  checkmate::assertDataFrame(si.input)
  checkmate::assertCharacter(primary.channel)
  checkmate::assertCharacter(secondary.channel)
  checkmate::assertNumeric(segment)
  
  
  # Calculate Secondary Stain Index --------------------------------------------
  ssi <- si.input[si.input$primary.channel == primary.channel &
                    si.input$secondary.channel == secondary.channel &
                    si.input$segment == segment,] |>
    dplyr::mutate(ssi = round((mfi.pos - mfi.neg)/(2*sd.neg), digits = 2)) |>
    dplyr::pull(ssi)
  
  
  # Generate output ------------------------------------------------------------
  return(ssi)
}