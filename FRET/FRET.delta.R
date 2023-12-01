# Load required packages
library(ggplot2)

# Function to perform structural sensitivity values
FRET.delta <- function(dir.fret, 
                       format.plot = "png",
                       xaxis_max = 0.5, 
                       xaxis_min = 0,
                       width = 5, 
                       height = 30) {
  
  # Obtain names for each construct
  temp_files <- list.files(dir.fret)
  
  # Create an empty vector to save information on mean, sd and names of the constructs
  temp_mean <- c()
  temp_sd <- c()
  temp_names <- c()
  
  # Analyze each construct
  for (bios in temp_files) {
    
    # Obtain the directory of each construct
    temp_path <- file.path(dir.fret, bios, "DATA", paste0(bios, ".csv"))
    
    # Verify if construct files exist
    if (file.exists(temp_path) == TRUE) {
      
      # Add names of each construct
      temp_names[length(temp_names) + 1] <- bios 
      
      # Load file
      fret_ratio <- read.csv(file = file.path(dir.fret, bios, "DATA", paste0(bios, ".csv")), 
                             header = TRUE)
      
      # Obtain data at 0 mM
      i <- fret_ratio[fret_ratio$Treatment == 0 & fret_ratio$Plate == 1, ]$Normalized
      
      # Obtain data at 1500 mM
      h <- fret_ratio[fret_ratio$Treatment == 1500, ]$Normalized
      
      # Obtain mean
      temp_mean[length(temp_mean) + 1] <- mean(h - i)
      
      # Obtain standard deviation
      temp_sd[length(temp_sd) + 1] <- sd(h - i)
      
    }
    
  }
  
  # Create a data frame for saving data
  fret_delta <- data.frame(matrix(nrow = length(temp_names), 
                                  ncol = 3))
  
  # Add names to the columns
  names(fret_delta)[1] <- "construct"
  names(fret_delta)[2] <- "mean_delta"
  names(fret_delta)[3] <- "sd_delta"
  
  
  # Add mean column
  fret_delta$mean_delta <- temp_mean
  
  # Add standard deviation column
  fret_delta$sd_delta <- temp_sd
  
  # Add names column
  fret_delta$construct <- temp_names
  
  # Save the file with the delta FRET data
  write.csv(x = fret_delta, 
            file = file.path(dir.fret, paste0("all_biosensors.csv")), 
            quote = FALSE, 
            row.names = FALSE)
  
  
  # Create a scatter plot with a standard deviation
  plot <- ggplot() +
    geom_vline(xintercept = unname(quantile(fret_delta$mean_delta)[2]), 
               lty = 2) +
    geom_vline(xintercept = median(fret_delta$mean_delta), 
               lty = 2, color = "blue") +
    geom_vline(xintercept = unname(quantile(fret_delta$mean_delta)[4]), 
               lty = 2) +
    geom_point(data = fret_delta, aes(x = mean_delta, 
                                      y = as.factor(construct)), 
               size = 4, shape = 21, 
               fill = "white", stroke = 1.5) +
    geom_errorbar(data = fret_delta, aes(x = mean_delta, 
                                         y = as.factor(construct), 
                                         xmin = mean_delta - sd_delta, 
                                         xmax = mean_delta + sd_delta), width = 0.2) +
    labs(x = expression(Delta * "FRET"), 
         y = "IDR") +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid = element_blank())  + 
    coord_cartesian(xlim = c(xaxis_min, xaxis_max))
  
  # Save plot
  ggsave(plot = plot, 
         filename = file.path(dir.fret, paste0("all_biosensors.", format.plot)), 
         device = format.plot, 
         width = width, height = height, units = "in", dpi = 450)
  
  
  # Show messages
  message("Don't forget to adjust the dimensions of the final plot with 'height' and 'width'\n")
  message("Default values")
  message("Width: 5 in")
  message("Heigth: 30 in")
  
}
