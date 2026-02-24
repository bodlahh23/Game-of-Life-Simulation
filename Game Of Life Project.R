# =========================================
# Conway's Game of Life Simulation (R)
# =========================================

# Load required libraries
library(ggplot2)
install.packages("reshape2")
library(reshape2)

# -------------------------
# Function: Count alive neighbors
# -------------------------
countNeighbors <- function(matrix, row, col) {
  alive_neighbors <- 0
  num_rows <- nrow(matrix)
  num_cols <- ncol(matrix)
  
  for (i in -1:1) {
    for (j in -1:1) {
      if (i == 0 && j == 0) next  # Skip the cell itself
      new_row <- row + i
      new_col <- col + j
      if (new_row >= 1 && new_row <= num_rows &&
          new_col >= 1 && new_col <= num_cols) {
        if (matrix[new_row, new_col] == TRUE) {
          alive_neighbors <- alive_neighbors + 1
        }
      }
    }
  }
  
  return(alive_neighbors)
}

# -------------------------
# Function: Update the matrix for the next generation
# -------------------------
updateCells <- function(matrix) {
  new_matrix <- matrix
  num_rows <- nrow(matrix)
  num_cols <- ncol(matrix)
  
  for (row in 1:num_rows) {
    for (col in 1:num_cols) {
      alive_neighbors <- countNeighbors(matrix, row, col)
      # Alive cell rules
      if (matrix[row, col] == TRUE) {
        if (alive_neighbors < 2 || alive_neighbors > 3) {
          new_matrix[row, col] <- FALSE
        }
      } else { # Dead cell rule
        if (alive_neighbors == 3) {
          new_matrix[row, col] <- TRUE
        }
      }
    }
  }
  return(new_matrix)
}

# -------------------------
# Function: Visualize the matrix
# -------------------------
visualizeMatrix <- function(mat, generation=1) {
  df <- melt(mat)
  colnames(df) <- c("Y", "X", "Alive")
  
  ggplot(df, aes(x=X, y=Y, fill=Alive)) +
    geom_tile(color="black") +
    scale_fill_manual(values=c("white","blue")) +
    theme_minimal() +
    scale_y_reverse() +
    labs(title=paste("Conway's Game of Life - Generation", generation),
         x="", y="") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
}

# -------------------------
# Function: Run simulation
# -------------------------
runSimulation <- function(initial_matrix, generations=10, pause=0.5) {
  current_matrix <- initial_matrix
  for (gen in 1:generations) {
    print(visualizeMatrix(current_matrix, gen))
    Sys.sleep(pause) # pause for animation effect
    current_matrix <- updateCells(current_matrix)
  }
}

# -------------------------
# Example: Initialize matrix
# Random TRUE/FALSE cells
# -------------------------
set.seed(123) # for reproducibility
initial_matrix <- matrix(sample(c(TRUE,FALSE), 25, replace=TRUE),
                         nrow=5, ncol=5)

# -------------------------
# Run simulation
# -------------------------
runSimulation(initial_matrix, generations=10, pause=30)