#' Generate a vector of colours from a colour code string
#'
#' This function takes a string of colour codes and returns a vector of corresponding hex colour values.
#' The function looks for colours set in "Options()" as "project_custom_colours".
#' The function supports four levels of intensity for each color family:
#' - Light colours prefixed with a period (e.g., .r)
#' - Medium colours represented by lowercase letters (e.g., r)
#' - Dark colours represented by uppercase letters (e.g., R)
#' - Darkest colours prefixed with an underscore and uppercase letter (e.g., _R)
#'
#' @param colour_str A character string containing color codes.
#'
#' @return A character vector of hex colour codes
#'
#' @examples
#' # Get medium red, dark red, medium blue, dark blue
#' gen_col("rRbB")  # Returns: c("#F8766D", "#b20b00", "#78ADFF", "#00348C")
#'
#' # Get all variations of blue from lightest to darkest
#' gen_col("-bbB+B")  # Returns: c("#d6e9ff", "#78ADFF", "#00348C", "#00246D")
#'
#' @export
gen_col <- function(colour_str) {
  
  colours <- getOption("project_custom_colours")
  
  # Helper function to process prefix keys
  check_prefix_key <- function(prefix, next_char, colours, result) {
    colour_key <- paste0(prefix, next_char)
    if (colour_key %in% names(colours)) {
      result <- c(result, colours[[colour_key]])
    } else {
      warning(paste0("Unknown colour code: ", colour_key))
    }
    return(result)
  }
  
  colour_adjustors <- c(
    "-", # Weaker
    "+" # Stronger
  )
  
  # Split the input string into individual characters
  colour_chars <- strsplit(colour_str, "")[[1]]
  
  # Process special cases for colours
  result <- c()
  i <- 1
  
  while (i <= length(colour_chars)) {
    # Check for specific prefixes 
    if (colour_chars[i] %in% colour_adjustors && i < length(colour_chars)) {
      # If true, combine
      colour_key <- paste0(colour_chars[i], colour_chars[i+1])
      # Check against colours and assign, otherwise warning.
      if (colour_key %in% names(colours)) {
        result <- c(result, colours[[colour_key]])
      } else {
        warning( paste0("Unknown colour code: ", colour_key) )
      }
      i <- i + 2
      next
    }
    
    # Process normal characters
    colour_key <- colour_chars[i]
    if (colour_key %in% names(colours)) {
      result <- c(result, colours[[colour_key]])
    } else {
      warning(paste0("Unknown colour code: ", colour_key))
    }
    
    i <- i + 1
  }
  
  return(result)
}