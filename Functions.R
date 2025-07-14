# Script for functions to support apical fluid analysis
# Sam Siljee
# 27th June 2025

# Function to get matrix offset values
get_alignment_offsets <- function(image_matrix, min_offset, max_offset) {
  # Set some variables
  n_rows <- nrow(image_matrix)

  # Take the average of the middle fifth as a reference row
  reference_row <- colMeans(image_matrix[floor(n_rows * 2 / 5):ceiling(n_rows * 3 / 5), ])

  # Initialise vector for offset results
  best_offsets <- numeric()

  # Loop through rows
  for (x in 1:n_rows) {
    # Initialise offset score for this row
    best_score <- Inf

    # Set image row
    image_row <- image_matrix[x, ]

    # Loop through offset values
    for (o in min_offset:max_offset) {
      # Calculate offset score
      score <- score_alignment(image_row, reference_row, o)
      
      # Compare calculated score to best score, and update if better
      if (score < best_score) {
        best_score <- score
        minimum_index <- o
      }
    }

    # Add minimum score in the minimum offsets vector
    best_offsets <- c(best_offsets, minimum_index)
  }
  
  # return the results
  return(best_offsets)
  
}

# Function to score row alignment
score_alignment <- function(image_row, reference_row, x_offset) {
  # Get original row length
  row_length <- length(image_row)

  # Create a row using the supplied offset
  if (x_offset > 0) { # Positive offset
    # Trim the end of the row
    offset_row <- image_row[1:(row_length - x_offset)]
  } else if (x_offset < 0) { # Negative offset
    # Trim the start of the row
    offset_row <- image_row[(-x_offset + 1):row_length]
  } else { # No offset
    offset_row <- image_row
  }

  # Create a reference row of the same length
  if (x_offset > 0) { # Positive offset
    # Trim the start of the row
    trimmed_reference_row <- reference_row[(x_offset + 1):row_length]
  } else if (x_offset < 0) { # Negative offset
    # Trim the end of the row
    trimmed_reference_row <- reference_row[1:(row_length + x_offset)]
  } else { # No offset
    trimmed_reference_row <- reference_row
  }

  # Return the mean absolute differences
  return(mean(abs(trimmed_reference_row - offset_row)))
}

# Function to align a matrix given an image matrix and set of offset values
align_matrix <- function(image_matrix, offsets) {
  # Set some values
  min_offset <- min(offsets)
  max_offset <- max(offsets)
  offset_difference <- abs(min_offset) + abs(max_offset)
  original_width <- ncol(image_matrix)
  
  # Calculate new width given offset values
  new_width <- original_width - offset_difference
  
  # Intialise new matrix
  aligned_matrix <- matrix(nrow = nrow(image_matrix), ncol = new_width)
  
  # Loop through rows
  for(x in 1:nrow(image_matrix)) {
    # Get offset value for that row
    offset_value <- offsets[x]
    
    # Get original row
    original_row <- image_matrix[x,]
    
    if(offset_value > 0) { # Positive offset
      # Take the shifted portion of the original row
      new_row <- original_row[(offset_value + 1):(offset_value + new_width)]
      
    } else if(offset_value < 0) { # Negative offset
      # Create new row with leading zeros, then add the original data
      new_row <- rep(0, new_width)
      new_row[(abs(offset_value) + 1):new_width] <- original_row[1:(new_width - abs(offset_value))]
      
    } else { # No offset
      # Take the middle portion of the original row
      start_col <- abs(min_offset) + 1
      new_row <- original_row[start_col:(start_col + new_width - 1)]
    }
    
    # Add the new row to the aligned matrix
    aligned_matrix[x,] <- new_row
  }
  
  # Return the aligned matrix
  return(aligned_matrix)
}

# Function to normalise the image for .png export
normalise_image <- function(input_matrix) {
  normalised_matrix <- (input_matrix - min(input_matrix)) / (max(input_matrix) - min(input_matrix))
  return(normalised_matrix)
}
