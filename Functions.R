# Script for functions to support apical fluid analysis
# Sam Siljee
# 27th June 2025

align_matrix <- function(image_matrix, max_x_offset) {
  # Take the middle row as a reference row
  reference_row <- image_matrix[round(nrow(image_matrix)/2),]
  
  # Define a function to check alignment
  check_alignment <- function(image_row, reference_row, x_offset) {
    # Create a row using the supplied offset
    if(x_offset == 0) {
      offset_row <- image_row
    } else if (x_offset > 0) {
      offset_row <- 
    } else {
      offset_row <- 
    }
  }
  
}
