# NOTE: ALL DIRECTORIES YOU HAVE TO WRITE IN THE FUNCTION, MUST BE
# WRITEN AND NOT JUST COPY AND PASTE. THIS AVOID THE APPEARENCE OF
# INDESIRABLE CHARACTERS. 

# Function for analyzing FRET data
FRET.data <- function(dir.input, 
                      dir.output, 
                      isosbestic = 515,
                      DxAm = 525,
                      DxDm = 475) {
  
  # Create a folder called "FRET" to save analized files 
  sub_dir <- "FRET" 
  temp_dir <- file.path(dir.output, sub_dir)
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Obtain the name of the folder where replicates are saved
  temp_files <- list.files(dir.input)
  
  # Obtain construct names
  for (i in temp_files) {
    
    if (i == temp_files[1]) {
      
      temp_rep_1 <- list.files(file.path(dir.input, i))
      
    }
    
    if (file.exists(temp_files[2])) {
      
      if (i == temp_files[2]) {
        
        temp_rep_2 <- list.files(file.path(dir.input, i))
        
      }
    }
    
    if (file.exists(temp_files[2])) {
      
      if (i == temp_files[3]) {
        
        temp_rep_3 <- list.files(file.path(dir.input, i))
        
      }
    }
  }
  
  # Create folders with the names of the analized constructs 
  for (i in temp_rep_1) {
    
    # Create subfolders for each construct 
    sub_dir <- i
    dir.create(file.path(temp_dir, sub_dir), 
               showWarnings = FALSE)
    
  }
  
  # Create two subfolders called "DATA" and "PLOTS" 
  for (i in temp_rep_1) {
    
    sub_dir <- "DATA"
    dir.create(file.path(dir.output, "FRET", i, sub_dir), 
               showWarnings = FALSE)
    
  }
  
  for (i in temp_rep_1) {
    
    sub_dir <- "PLOTS"
    dir.create(file.path(dir.output, "FRET", i, sub_dir), 
               showWarnings = FALSE)
    
  }
  
  
  # Read each file from constructs folder 
  for (bios in temp_rep_1) {
    for (reps in temp_files) {
      for (plate in c(1, 2)) {
        
        
        # Verify in directory exists 
        temp_path <- file.path(dir.input, reps, bios, 
                               paste0(plate, ".xlsx"))
        
        if (file.exists(temp_path) == TRUE) {
          
          # Load EXCEL file 
          fret <- readxl::read_excel(path = file.path(dir.input, reps, bios, 
                                                      paste0(plate, ".xlsx")),
                                     sheet = 1, 
                                     col_names = FALSE)[-c(1:9), ]
          
          # Add names to the columns for their identification 
          names(fret)[1:length(names(fret))] <- unlist(fret[2, ], use.names = FALSE)
          
          # Remove first row 
          fret <- fret[-c(1, 2), ]
          
          # Create a data frame with the number of columns corresponding to the construct for analyzing 
          temp_fret <- data.frame(matrix(ncol = (length(fret[, -c(1, 2)])/(3)), 
                                         nrow = nrow(fret)))
          temp_vec <- c()
          for (a in 1:length(temp_fret)) {
            
            temp_name <- paste0("Condition_", a)
            temp_vec[length(temp_vec) + 1] <- temp_name
            
          }
          
          # Add specific names for each column of the data frame created before 
          colnames(temp_fret) <- temp_vec
          
          # Obtain mean for each condition performed for every wavelength measured
          ex.max <- as.integer(fret[nrow(fret), 2])
          ex.min <- as.integer(fret[1, 2])
          
          # Calculate the range of wavelengths of the experiment
          wl <- ((ex.max - ex.min) + 1)
          
          # Create a new empty data frame
          temp_df <- data.frame(matrix(ncol = 4, nrow = wl))
          for (a in (seq(from = 1, to = length(fret[, -c(1, 2)]), by = 3) + 2)) {
            
            # Obtain the values for each wavelength for each condition 
            temp <- as.numeric(unlist((fret[, a])))
            temp_df[, 1] <- temp
            
            temp <- as.numeric(unlist((fret[, (a + 1)])))
            temp_df[, 2] <- temp
            
            temp <- as.numeric(unlist((fret[, (a + 2)])))
            temp_df[, 3] <- temp
            
            # Calculate mean the three observations (each condition)
            i <- (temp_df[, 1] + temp_df[, 2] + temp_df[, 3])
            h <- (length(temp_df)-1)
            
            # Calculate mean of the conditions 
            temp <- (i/h)
            temp_df[, 4] <- temp
            
            # Add each column with each mean to the data frame called "temp_fret" 
            temp_fret[, (a/3)] <- temp_df[, 4]
            
          }
          
          # Add the wavelength in the first column 
          temp_fret[, length(temp_fret) + 1] <- fret$`Wavelength [nm]`
          temp_fret <- temp_fret[, c(length(temp_fret), 1:(length(temp_fret) - 1))]
          names(temp_fret)[1] <- names(fret)[2]
          
          # Normalize fluorescence with the isosbestic point of the FRET pair 
          temp <- (t(temp_fret[temp_fret[, 1] == as.character(isosbestic),
                               c(2:length(temp_fret))]))
          
          # Remove row names 
          rownames(temp) <- NULL
          
          # Normalize each value of data frame "temp_fret" using the vector "temp" 
          temp_fret_N <- data.frame(matrix(ncol = (length(fret[, -c(1,2)])/(3)), 
                                           nrow = nrow(fret)))
          temp_vec <- c()
          for (a in 1:length(temp_fret_N)) {
            
            temp_name <- paste0("Condition_", a)
            temp_vec[length(temp_vec) + 1] <- temp_name
            
          }
          
          
          if (plate == 1) {
            
            # Add specific names for each of the columns of the data frame
            colnames(temp_fret_N) <- temp_vec
            
          }
          
          if (plate == 2) {
            
            # Chanage names of the columns
            temp_vec[1] <- "Condition_5"
            temp_vec[2] <- "Condition_6"
            temp_vec[3] <- "Condition_7"
            temp_vec[4] <- "Condition_8"
            
            # Add specific names for each of the columns of the data frame
            colnames(temp_fret_N) <- temp_vec
            
          }
          
          # Normalize data
          for (a in 1:nrow(temp_fret)) {
            
            i <- (temp_fret[a, -1])/(temp)
            temp_fret_N[a, ] <- i
            
          }
          
          # Add wavelength to the first column
          temp_fret_N[, length(temp_fret_N) + 1] <- fret$`Wavelength [nm]`
          temp_fret_N <- temp_fret_N[, c(length(temp_fret_N), 1:(length(temp_fret_N) - 1))]
          names(temp_fret_N)[1] <- names(fret)[2]
          
          # Add column with the replicates
          temp_fret_N$Replicate <- reps
          
          # Add colum with the name of the plate (if you use two plates)
          temp_fret_N$Plate <- plate
          
          # Add column with the names of the construct
          temp_fret_N$Construct <- bios
          
          # Save files in the subfolder "DATA" for each construct performed
          write.csv(x = temp_fret_N,
                    file = file.path(dir.output, "FRET", bios, "DATA", 
                                     paste0("sptr_", bios, "_", reps, "_", 
                                            "plate", plate, ".csv")
                    ),
                    row.names = FALSE, 
                    quote = FALSE
          )
          
          
          
          # Obtain DxAm and DxDm ratio and extract the number of constructs
          i <- length(fret[, -c(1, 2)])/12 # Cantidad de constructos
          
          # Generate a data frame to save the processed fluorescence values
          temp_df <- data.frame(matrix(ncol = (length(fret[, -c(1, 2)])/i), 
                                       nrow = 0))
          
          # Obtain rows with the fluorescences of donor and acceptor
          temp_rep <- fret[fret$`Wavelength [nm]` == as.character(DxDm) | 
                             fret$`Wavelength [nm]` == as.character(DxAm), ]
          
          # Create an empty vector
          temp_vec <- c()
          for (a in seq(1, length(fret[, -c(1, 2)]), 12)) {
            
            temp_vec <- temp_rep[, seq(a + 2, a + 13)]
            temp_df[nrow(temp_df) + 1, ] <- temp_vec[1, ] 
            temp_df[nrow(temp_df) + 1, ] <- temp_vec[2, ] 
            
          }
          
          temp_df[, length(temp_df) + 1] <- c(as.character(DxDm), as.character(DxAm))
          temp_df <- temp_df[, c(length(temp_df), (1:length(temp_df) - 1))]
          names(temp_df)[1] <- names(fret)[2]
          
          # Create names according to the plate used (1 or 2)
          if (plate == 1) {
            
            # Create names for the osmolytes
            temp_name <- c(rep(paste0("NaCl_", "0mM"), 3), rep(paste0("NaCl_", "200mM"), 3),
                           rep(paste0("NaCl_", "400mM"), 3), rep(paste0("NaCl_", "600mM"), 3))
            
            # Add names to the columns for each condition
            names(temp_df)[2:length(temp_df)] <- temp_name
            
          }
          
          if (plate == 2) {
            
            # Create names
            temp_name <- c(rep(paste0("NaCl_", "0mM"), 3), rep(paste0("NaCl_", "800mM"), 3),
                           rep(paste0("NaCl_", "1000mM"), 3), rep(paste0("NaCl_", "1500mM"), 3))
            
            # Add names to the columns for each condition
            names(temp_df)[2:length(temp_df)] <- temp_name
            
          }
          
          # Generate an empty data frame to stored the normalized data
          temp_rep <- data.frame(matrix(ncol = 9, 
                                        nrow = (12*length(fret[, -c(1,2)])/12)))
          
          # Add names to teh columns to the new data frame created
          names(temp_rep)[1] <- "Treatment"
          names(temp_rep)[2] <- "DxAm"
          names(temp_rep)[3] <- "DxDm"
          names(temp_rep)[4] <- "DxAm/DxDm"
          names(temp_rep)[5] <- "Mean"
          names(temp_rep)[6] <- "Normalized"
          names(temp_rep)[7] <- "Replicate"
          names(temp_rep)[8] <- "Construct"
          names(temp_rep)[9] <- "Plate"
          
          
          # Create an if statement for obtaining the treatment (osmolyte concentrations)
          if (plate == 1) {
            
            # Add the "Treatment" column
            temp_rep$Treatment <- rep(c(0, 200, 400, 600), each = 3)
            temp_rep$Plate <- 1
          }
          
          if (plate == 2) {
            
            # Add the "Treatment" column
            temp_rep$Treatment <- rep(c(0, 800, 1000, 1500), each = 3)
            temp_rep$Plate <- 2
            
          }
          
          
          # Add DxAm column extracting from "temp_df" all data at 525 wavelength
          temp_vec_525 <- c() # Create an empty vector
          temp_vec <- temp_df[seq(2, nrow(temp_df), by = 2), -1]
          for (a in 1:nrow(temp_df[seq(2, nrow(temp_df), by = 2), -1])) {
            
            temp_vec_525 <- c(temp_vec_525, as.numeric(temp_vec[a, ]))
            
          }
          
          # Add DxAm column extracting from "temp_df" all data at 475 wavelength
          temp_vec_475 <- c() # Create an empty vector
          temp_vec <- temp_df[seq(1, nrow(temp_df), by = 2), -1]
          for (a in 1:nrow(temp_df[seq(1, nrow(temp_df), by = 2), -1])) {
            
            temp_vec_475 <- c(temp_vec_475, as.numeric(temp_vec[a, ]))
            
          }
          
          # Add final columns of DxAm and DxDm
          temp_rep$DxAm <- temp_vec_525
          temp_rep$DxDm <- temp_vec_475
          
          # Perform and add the FRET ratio DxAm/DxDm
          temp_rep$`DxAm/DxDm` <- temp_vec_525/temp_vec_475
          
          # Add mean of FRET ratio from the treatment at 0 mM for each construct, extracting
          # all data from 0 mM
          h <- temp_rep[seq(1, nrow(temp_rep), 12), 4]
          i <- temp_rep[(seq(1, nrow(temp_rep), 12) + 1), 4]
          j <- temp_rep[(seq(1, nrow(temp_rep), 12) + 2), 4]
          
          # Perform the mean of the data values at 0 mM and create 12 reps for each construct
          temp_rep$Mean <- rep((h + i + j)/(3), each = 12)
          
          # Divide each mean value with the value of FRET ratio for each construct and condition
          temp_rep$Normalized <- (temp_rep$`DxAm/DxDm`)/(temp_rep$Mean)
          
          # Add working replicate 
          temp_rep$Replicate <- as.numeric(substr(start = nchar(reps), 
                                                  stop = nchar(reps), 
                                                  x = reps))
          
          # Add analyzed construct
          temp_rep$Construct <- bios
          
          # Create six variables for storing the data for each construct in a independent way 
          if (reps == temp_files[1] && plate == 1) {
            
            temp_rep1 <- temp_rep
            
          } 
          
          if (reps == temp_files[1] && plate == 2) {
            
            temp_rep2 <- temp_rep
            
          } 
          
          
          if (!is.na(temp_files[2]) == TRUE) {
            if (reps == temp_files[2] && plate == 1) {
              
              temp_rep3 <- temp_rep
              
            }
          }
          
          
          if (!is.na(temp_files[2]) == TRUE) {
            if (reps == temp_files[2] && plate == 2) {
              
              temp_rep4 <- temp_rep
              
            }
          }
          
          
          if (!is.na(temp_files[3]) == TRUE) {
            if (reps == temp_files[3] && plate == 1) {
              
              temp_rep5 <- temp_rep
              
            }
          }
          
          
          if (!is.na(temp_files[3]) == TRUE) {
            if (reps == temp_files[3] && plate == 2) {
              
              temp_rep6 <- temp_rep
              
            }
          }
          
          
          if (exists("temp_rep6") == TRUE) {
            
            # Merge data frames with all data for every replicate performed
            fret_all <- rbind(temp_rep1, temp_rep2, temp_rep3, 
                              temp_rep4, temp_rep5, temp_rep6)
            
            # Save final file with all the data for each construct in the subfolder "DATA" 
            write.csv(x = fret_all,
                      file = file.path(dir.output, "FRET", bios, "DATA",
                                       paste0(bios, ".csv")
                      ),
                      row.names = FALSE,
                      quote = FALSE 
            )
            
            # Remove variable "fret_all" for analyzing next construct
            if (exists("fret_all") == TRUE) {
              
              remove(... = fret_all)
              
            }
            
          } else if ((exists("temp_rep4") & exists("temp_rep3")) == TRUE) {
            
            # Merge data frames with all data for every replicate performed
            fret_all <- rbind(temp_rep1, temp_rep2, 
                              temp_rep3, temp_rep4)
            
            # Save final file with all the data for each construct in the subfolder "DATA" 
            write.csv(x = fret_all,
                      file = file.path(dir.output, "FRET", bios, "DATA",
                                       paste0(bios, ".csv")
                      ),
                      row.names = FALSE,
                      quote = FALSE 
            )
            
            # Remove variable "fret_all" for analyzing next construct
            if (exists("fret_all") == TRUE) {
              
              remove(... = fret_all)
              
            }
            
          } else if ((exists("temp_rep1") & exists("temp_rep2")) == TRUE) {
            
            # Merge data frames with all data for every replicate performed
            fret_all <- rbind(temp_rep1, temp_rep2)
            
            # Save final file with all the data for each construct in the subfolder "DATA" 
            write.csv(x = fret_all,
                      file = file.path(dir.output, "FRET", bios, "DATA",
                                       paste0(bios, ".csv")
                      ),
                      row.names = FALSE,
                      quote = FALSE 
            )
            
            # Remove variable "fret_all" for analyzing next construct
            if (exists("fret_all") == TRUE) {
              
              remove(... = fret_all)
              
            }
          }
          
        } else {
          
          next
          
        }
      }
    } 
  }
  
  
  # Show a brief overview of the data analysis
  message("Overview of the analysis:")
  message("==============================================\n")
  
  message(paste("isosbestic point: ", isosbestic, "nm"))
  message(paste("Acceptor fluorescence (DxAm): ", DxAm, "nm"))
  message(paste("Donor fluorescence (DxDm): ", DxDm, "nm"))
  
  message("\n==============================================")
  
}

# End of analysis
