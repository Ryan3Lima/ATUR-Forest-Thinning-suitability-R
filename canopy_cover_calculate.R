# Load necessary libraries
library(ggplot2)
library(gridExtra)



calculate_starting_canopy <- function(ending_canopy, percent_reduction) {
  # Validate inputs
  if (percent_reduction < 0 || percent_reduction >= 100) {
    stop("Percent reduction must be between 0 and 100.")
  }
  if (ending_canopy < 0) {
    stop("Ending canopy cover must be non-negative.")
  }
  
  # Calculate the retained fraction
  retained_fraction <- 1 - (percent_reduction / 100)
  
  # Calculate the starting canopy cover
  starting_canopy <- ending_canopy / retained_fraction
  
  return(starting_canopy)
}

# Example usage
ending_canopy <- 16  # Desired ending canopy cover percentage
percent_reduction <- 20  # Percentage reduction
starting_canopy <- calculate_starting_canopy(ending_canopy, percent_reduction)
cat(sprintf("The starting canopy cover should be at least %.2f%%.\n", starting_canopy))


# Set parameters
reduction_percentage <- 20
retained_fraction <- 1 - (reduction_percentage / 100)

# Define initial canopy cover range (e.g., 0% to 100%)
initial_canopy <- seq(0, 100, by = 1)

# Calculate final canopy cover after reduction
final_canopy <- initial_canopy * retained_fraction

# Smooth suitability function using a polynomial
calculate_suitability <- function(final_canopy) {
  if (final_canopy <= 16 || final_canopy >= 80) {
    return(0)  # Suitability is 0 outside these bounds
  }
  
  # Polynomial fit: peaks at 37, decreases smoothly toward 16 and 80
  median <- 37
  if (final_canopy < median) {
    return(10 * (1 - ((final_canopy - median) / (16 - median))^2))  # Left-hand curve
  } else {
    return(10 * (1 - ((final_canopy - median) / (80 - median))^2))  # Right-hand curve
  }
}

# Apply suitability function to final canopy cover
suitability <- sapply(final_canopy, calculate_suitability)

# Prepare data for plotting
data1 <- data.frame(Initial = initial_canopy, Final = final_canopy)
data2 <- data.frame(Final = final_canopy, Suitability = suitability)

# Plot 1: Initial canopy vs. final canopy
plot1 <- ggplot(data1, aes(x = Initial, y = Final)) +
  geom_line(color = 'black', size = 1.2) +
  labs(
    title = 'Initial vs. Final Canopy Cover with 20% reduction',
    x = 'Initial Canopy Cover (%)',
    y = 'Final Canopy Cover (%)'
  ) +
  theme_minimal(base_family = "serif") +  # Use a serif font (e.g., Times New Roman)
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Plot 2: Suitability vs. final canopy
plot2 <- ggplot(data2, aes(x = Final, y = Suitability)) +
  geom_line(color = 'black', size = 1.2) +
  labs(
    title = 'Suitability vs. Final Canopy Cover',
    x = 'Final Canopy Cover (%)',
    y = 'Suitability (1-10)'
  ) +
  theme_minimal(base_family = "serif") +  # Use a serif font (e.g., Times New Roman)
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Combine the plots into one figure
combined_plot <- grid.arrange(plot1, plot2, ncol = 1)

# Display the combined plot
print(combined_plot)
