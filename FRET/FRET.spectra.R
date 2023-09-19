# Load requiered packages
library(ggplot2)

# Function for creating spectra plots
FRET.spectrum <- function(dir.fret, 
                          format.plot = "png",
                          ymax_name = 1.8, 
                          ymax_axis = 2, 
                          ymin_axis = 0.3,
                          yaxis_ticks = 0.2, 
                          bios_name = TRUE) {
  
  
  # Obtain name of the constructs analyzed and save in the FRET folder (Run FRET.data script)
  temp_files_n <- list.files(dir.fret)
  
  #Load files
  for (bios in temp_files_n) {
    
    # Obtain name of each construct 
    temp_files <- list.files(path = file.path(dir.fret, bios, "DATA"), 
                             pattern = "sptr")
    
    
    # Load data from plate 1 and replicate 1 and verify if file exists
    temp_path_1 <- file.path(dir.fret, bios, "DATA", temp_files[1])
    if (file.exists(temp_path_1) == TRUE) {
      
      # Load file
      sptr_1 <- read.csv(file = file.path(dir.fret, bios, "DATA", temp_files[1]),
                         header = TRUE)
      
      # Remove columns to avoid data duplications
      sptr_1$Replicate <- NULL
      sptr_1$Plate <- NULL
      sptr_1$Construct <- NULL
      
    } 
    
    
    # Verifiy if file exists
    temp_path_2 <- file.path(dir.fret, bios, "DATA", temp_files[2])
    if (file.exists(temp_path_2) == TRUE) {
      
      # Load data from plate 2 and replicate 1
      sptr_2 <- read.csv(file = file.path(dir.fret, bios, "DATA", temp_files[2]),
                         header = TRUE)
      
      # Remove columns to avoid data duplications
      sptr_2$Wavelength..nm. <- NULL
      
    } 
    
    
    # Verifiy if file exists
    temp_path_3 <- file.path(dir.fret, bios, "DATA", temp_files[3])
    if (file.exists(temp_path_3) == TRUE) {
      
      # Load data from plate 1 and replicate 2
      sptr_3 <- read.csv(file = file.path(dir.fret, bios, "DATA", temp_files[3]),
                         header = TRUE)
      
      # Remove columns to avoid data duplications
      sptr_3$Replicate <- NULL
      sptr_3$Plate <- NULL
      sptr_3$Construct <- NULL
      
    } 
    
    
    # Verifiy if file exists
    temp_path_4 <- file.path(dir.fret, bios, "DATA", temp_files[4])
    if (file.exists(temp_path_4) == TRUE) {
      
      # Load data from plate 2 and replicate 2
      sptr_4 <- read.csv(file = file.path(dir.fret, bios, "DATA", temp_files[4]),
                         header = TRUE)
      
      # Remove columns to avoid data duplications
      sptr_4$Wavelength..nm. <- NULL
      
    } 
    
    
    # Verifiy if file exists
    temp_path_5 <- file.path(dir.fret, bios, "DATA", temp_files[5])
    if (file.exists(temp_path_5) == TRUE) {
      
      # Load data from plate 1 and replicate 3
      sptr_5 <- read.csv(file = file.path(dir.fret, bios, "DATA", temp_files[5]),
                         header = TRUE)
      
      # Remove columns to avoid data duplications
      sptr_5$Replicate <- NULL
      sptr_5$Plate <- NULL
      sptr_5$Construct <- NULL
      
    } 
    
    
    # Verifiy if file exists
    temp_path_6 <- file.path(dir.fret, bios, "DATA", temp_files[6])
    if (file.exists(temp_path_6) == TRUE) {
      
      # Load data from plate 2 and replicate 3
      sptr_6 <- read.csv(file = file.path(dir.fret, bios, "DATA", temp_files[6]),
                         header = TRUE)
      
      # Remove columns to avoid data duplications
      sptr_6$Wavelength..nm. <- NULL
      
    } 
    
    
    # Create spectra plot colors
    color_lines <- c("#add8e6", "#92b8dc", "#7698d1", 
                     "#5b78c7", "#3f58bd", "#2438b2", 
                     "#0818a8")
    
    # Create the legend of the spectra plot to be displayed
    NaCl_lines <- c("0", "0.2", "0.4", "0.6", "0.8", "1.0", "1.5")
    
    # Merge data frames of the same replicate
    
    # Verify if exists data frame with replicates 1 and 2
    if (exists("sptr_1") & exists("sptr_2") == TRUE) {
      
      # Data from replicate 1
      Datos de réplica 1
      temp_fret_N <- cbind(sptr_1, sptr_2)
      
      # Create spectra plot
      plot_sptr <- ggplot(data = temp_fret_N[seq(1, nrow(temp_fret_N), 5), ]) + 
        # NaCl_0 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_1,
                      color = "0"), size = 0.8) +
        # NaCl_200 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_2,
                      color = "0.2"), size = 0.8) +
        # NaCl_400 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_3,
                      color = "0.4"), size = 0.8) +
        # NaCl_600 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_4,
                      color = "0.6"), size = 0.8) +
        # NaCl_800 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_6,
                      color = "0.8"), size = 0.8) +
        # NaCl_1000 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_7,
                      color = "1.0"), size = 0.8) +
        # NaCl_1000 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_8,
                      color = "1.5"), size = 0.8) +
        # Add modifications and theme layers 
        theme_bw() +
        labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              panel.grid = element_blank()) +
        coord_cartesian(ylim = c(ymin_axis, ymax_axis)) +
        scale_y_continuous(breaks = seq(ymin_axis, ymax_axis, yaxis_ticks)) +
        scale_color_manual(name = "[NaCl] (M)",
                           values = color_lines, 
                           label = NaCl_lines) +
        
        # Add if statement for adding the name of the construct
        if (bios_name == TRUE) {
          
          # For adding the desired name 
          Colocar nombre del número del biosensor  
          annotate(geom = "text", x = 505, y = ymax_name, 
                   label = paste0("IDRBS-", bios), size = 4.5)
          
        } else {
          
          # For adding the name put in the folder FRET
          annotate(geom = "text", x = 505, y = ymax_name, 
                   label = bios, size = 4.5)
          
        }
      
      # Generate name of the plot to be saved
      name_plot <- paste0("plot_sptr_", bios, "_", temp_fret_N$Replicate[1], ".",
                          format.plot)
      
      # Save plot
      ggsave(plot = plot_sptr, 
             filename = file.path(dir.fret, bios, "PLOTS", name_plot), 
             device = format.plot, width = 6, height = 4, units = "in", 
             dpi = 450)
      
      # Remove variable temp_fret_N for analyzing next constructs
      remove(... = temp_fret_N)
      
    } # End replicate 1
    
    
    # Verify if exists data frame with replicate 2
    if (exists("sptr_3") & exists("sptr_4") == TRUE) {
      
      # Data from replicate 2
      temp_fret_N <- cbind(sptr_3, sptr_4)
      
      # Create spectra plot
      plot_sptr <- ggplot(data = temp_fret_N[seq(1, nrow(temp_fret_N), 5), ]) + 
        # NaCl_0 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_1,
                      color = "0"), size = 0.8) +
        # NaCl_200 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_2,
                      color = "0.2"), size = 0.8) +
        # NaCl_400 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_3,
                      color = "0.4"), size = 0.8) +
        # NaCl_600 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_4,
                      color = "0.6"), size = 0.8) +
        # NaCl_800 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_6,
                      color = "0.8"), size = 0.8) +
        # NaCl_1000 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_7,
                      color = "1.0"), size = 0.8) +
        # NaCl_1000 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_8,
                      color = "1.5"), size = 0.8) +
        # Add modifications and theme layers 
        theme_bw() +
        labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              panel.grid = element_blank()) +
        coord_cartesian(ylim = c(ymin_axis, ymax_axis)) +
        scale_y_continuous(breaks = seq(ymin_axis, ymax_axis, yaxis_ticks)) +
        scale_color_manual(name = "[NaCl] (M)",
                           values = color_lines, 
                           label = NaCl_lines) +
        
        # Add if statement for adding the name of the construct
        if (bios_name == TRUE) {
          
          # For adding the desired name 
          annotate(geom = "text", x = 505, y = ymax_name, 
                   label = paste0("IDRBS-", bios), size = 4.5)
          
        } else {
          
          # For adding the name put in the folder FRET
          annotate(geom = "text", x = 505, y = ymax_name, 
                   label = bios, size = 4.5)
          
        }
      
      # Generate name of the plot to be saved
      name_plot <- paste0("plot_sptr_", bios, "_", temp_fret_N$Replicate[1], ".",
                          format.plot)
      
      # Save plot
      ggsave(plot = plot_sptr, 
             filename = file.path(dir.fret, bios, "PLOTS", name_plot), 
             device = format.plot, width = 6, height = 4, units = "in", 
             dpi = 450)
      
      # Remove variable temp_fret_N for analyzing next constructs
      remove(... = temp_fret_N)
      
    } # End replicate 2
    
    # Verify if exists data frame with replicate 3
    if (exists("sptr_5") & exists("sptr_6") == TRUE) {
      
      # Data from replicate 3
      temp_fret_N <- cbind(sptr_5, sptr_6)
      
      # Create spectra plot
      plot_sptr <- ggplot(data = temp_fret_N[seq(1, nrow(temp_fret_N), 5), ]) + 
        # NaCl_0 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_1,
                      color = "0"), size = 0.8) +
        # NaCl_200 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_2,
                      color = "0.2"), size = 0.8) +
        # NaCl_400 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_3,
                      color = "0.4"), size = 0.8) +
        # NaCl_600 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_4,
                      color = "0.6"), size = 0.8) +
        # NaCl_800 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_6,
                      color = "0.8"), size = 0.8) +
        # NaCl_1000 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_7,
                      color = "1.0"), size = 0.8) +
        # NaCl_1000 mM
        geom_line(aes(x = as.numeric(Wavelength..nm.), y = Condition_8,
                      color = "1.5"), size = 0.8) +
        # Add modifications and theme layers 
        theme_bw() +
        labs(x = "wavelength (nm)", y = "normalized\nfluorescence") +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              panel.grid = element_blank()) +
        coord_cartesian(ylim = c(ymin_axis, ymax_axis)) +
        scale_y_continuous(breaks = seq(ymin_axis, ymax_axis, yaxis_ticks)) +
        scale_color_manual(name = "[NaCl] (M)",
                           values = color_lines, 
                           label = NaCl_lines) +
        
        # Add if statement for adding the name of the construct
        if (bios_name == TRUE) {
          
          # For adding the desired name 
          annotate(geom = "text", x = 505, y = ymax_name, 
                   label = paste0("IDRBS-", bios), size = 4.5)
          
        } else {
          
          # For adding the name put in the folder FRET
          annotate(geom = "text", x = 505, y = ymax_name, 
                   label = bios, size = 4.5)
          
        }
      
      # Generate name of the plot to be saved
      name_plot <- paste0("plot_sptr_", bios, "_", temp_fret_N$Replicate[1], ".",
                          format.plot)
      
      # Save plot
      ggsave(plot = plot_sptr, 
             filename = file.path(dir.fret, bios, "PLOTS", name_plot), 
             device = format.plot, width = 6, height = 4, units = "in", 
             dpi = 450)
      
      # Remove variable temp_fret_N for analyzing next constructs
      remove(... = temp_fret_N)
      
    } # End replicate 3
  }
  
  # Show messagges if analysis was succed
  message("Plots created with succed")
  
}
