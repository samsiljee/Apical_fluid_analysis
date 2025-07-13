# Script for functions to support apical fluid analysis
# Sam Siljee
# 27th June 2025

# Function to get matrix offset values
get_alignment_offsets <- function(image_matrix, max_offset, min_offset) {
  # Set some variables
  n_rows <- nrow(image_matrix)
  
  # Take the average of the middle fifth as a reference row
  reference_row <- colMeans(image_matrix[round(n_rows*2/5):round(n_rows*3/5),])
  
  # Loop through rows
  for(x in n_rows){
    # Set image row
    image_row <- image_matrix[x,]
    
    # Loop through offset values
    for(o in min_offset:max_offset) {
      score_alignment(image_row, reference_row, o)
    }
    
  }
  
}

# Function to score row alignment
score_alignment <- function(image_row, reference_row, x_offset) {
  # Get original row length
  row_length <- length(image_row)
  
  # Create a row using the supplied offset
  if(x_offset == 0) {
    offset_row <- image_row
  } else if (x_offset > 0) {
    # Trim the start of the row
    offset_row <- image_row[(-x_offset+1):row_length]
  } else {
    # Trim the end of the row
    offset_row <- image_row[1:(row_length-x_offset)]
  }
  
  # Create a reference row of the same length
  if(x_offset == 0) {
    trimmed_reference_row <- reference_row
  } else if (x_offset > 0) {
    # Trim the end of the row
    trimmed_reference_row <- reference_row[1:(row_length-x_offset)]
  } else {
    # Trim the start of the row
    trimmed_reference_row <- reference_row[(-x_offset+1):row_length]
  }
  
  # Return the mean absolute differences
  return(mean(abs(trimmed_reference_row - offset_row)))
}
