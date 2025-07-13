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
  for (x in n_rows) {
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

    # Store minimum score in the minimum offsets vector
    best_offsets[x] <- minimum_index
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
