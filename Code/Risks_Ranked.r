library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(reshape2)
library(epitools)
library(readxl)
library(tidyverse)
library(readr)
library(arsenal)
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")
library(patchwork)
library(palmerpenguins)
library(viridis)
library(gt)
library(gtExtras)
library(RColorBrewer)
library(reshape2)
library(knitr)
library(vtable)
library(ggrepel)

#####Setup#####
#--------------any libraries needed are loaded and displayed below--------------
#
library(dplyr)
library(zoo)
library(kableExtra)
#
#--------------make project folders and folder paths----------------------------

library(httpgd)
hgd()
hgd_browse()
wd <- getwd()  # working directory

folders <- c("Data Output", "Figures")
# function to create folders below
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == FALSE)
    dir.create(folders[i])
}


# we also need to store the paths to these new folders
data.output.path <- paste(wd, "/", folders[1], sep = "")
figures.path <- paste(wd, "/", folders[2], sep = "")


# now we can access and save stuff to these folders!



#---------------------Below, we upload and clean the data----------


#first, lets load in out data. Do this either by selecting the drop down menu
#go to file, import data, import data Future_Bio_Riskom excel, and import your data
#copy and paste the output Future_Bio_Riskom the console tab below, like I did here
#alternativley, just run my code below
#make sure you name and assign your new spreadsheet, below I assign this import
#by naming it "nuseds" below, now when I use the name nuseds, it will be called
#on in the program
data.path <- paste(wd, "/", "Data", sep = "")

####################CU and DU Risk Tables ####################
FWRA <- read_excel(paste(data.path, "FWRA_2021_RESULTS_MASTER_Original.xlsx", sep = "/"), sheet = 1)

FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#Remove all LFs in the estuary from Muriel Lake, Upper Kennedy, Sand River, Clayoquot River (these sub-basins don't have an estuary)


#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"

#add column SMU and replicate "SMU" for all rows
FWRA$SMU <- "WVI"

####Risk Ranked Across All LFs and Spatial Scales####
library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"



# Define a function to add missing columns to a data frame and fill them with 0s
add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% colnames(df)) {
      df[[column]] <- 0
    }
  }
  return(df)
}

# Define a function to process filtered data: calculate proportions, sort risks, and save to a CSV file
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  risk_summary <- filtered_data %>%
    mutate(VH = ifelse(!!sym(risk_column) %in% c("VH"), 1, 0),
           H = ifelse(!!sym(risk_column) %in% c("H"), 1, 0),
           M = ifelse(!!sym(risk_column) %in% c("M"), 1, 0),
           L = ifelse(!!sym(risk_column) %in% c("L"), 1, 0),
           VL = ifelse(!!sym(risk_column) %in% c("VL"), 1, 0), 
           LPDG = ifelse(!!sym(risk_column) %in% c("LPDG"), 1, 0),
           HPDG = ifelse(!!sym(risk_column) %in% c("HPDG"), 1, 0)) %>%
            
    group_by(LF_Number,LF_Name) %>%
    summarise(VH_total_count = sum(VH),
              H_total_count = sum(H),
              M_total_count = sum(M),
              L_total_count = sum(L),
              VL_total_count = sum(VL),
              LPDG_total_count = sum(LPDG),
              HPDG_total_count = sum(HPDG),
              total_count = n()) %>% # Count the total number of rows in the filtered_data
    mutate(VH_total_prop = VH_total_count / total_count,
           H_total_prop = H_total_count / total_count,
           M_total_prop = M_total_count / total_count,
           L_total_prop = L_total_count / total_count,
           VL_total_prop = VL_total_count / total_count,
           LPDG_total_prop = LPDG_total_count / total_count,
           HPDG_total_prop = HPDG_total_count / total_count)

  # Sort the risk summary data frame by the highest proportion for each risk level in the order of risk levels
  sorted_risks <- risk_summary %>%
    arrange(desc(VH_total_prop), desc(H_total_prop), desc(M_total_prop),desc(HPDG_total_prop), desc(L_total_prop), desc(VL_total_prop), desc(LPDG_total_prop)) %>%
    select(LF_Number, LF_Name,total_count, VH_total_prop, VH_total_count, H_total_prop, H_total_count, M_total_prop, M_total_count, HPDG_total_prop, HPDG_total_count, L_total_prop, L_total_count, VL_total_prop, VL_total_count, LPDG_total_prop, LPDG_total_count)

  # Print the sorted risks data frame to the console
  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  # Save the sorted risks data frame to a CSV file
  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_risks_", file_suffix, ".csv"))
}


# Get unique values for LF_Number, CU_ACRO, Area, and SYSTEM_SITE
unique_lf_numbers <- unique(FWRA$LF_Number)
unique_cu_acros <- unique(FWRA$CU_ACRO)
unique_areas <- unique(FWRA$Area)
unique_system_sites <- unique(FWRA$SYSTEM_SITE)
unique_SMU <- unique(FWRA$SMU)

# Loop for SMU current risk
    for (smu in unique_SMU) {
      filtered_data <- FWRA %>%
        filter(SMU == smu)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("SMU_Current", smu, sep="_"))
    }

# Loop for SMU future risk
    for (smu in unique_SMU) {
      filtered_data <- FWRA %>%
        filter(SMU == smu)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("SMU_Future", smu, sep="_"))
    }

# Loop for CU_ACRO current risk
    for (cu_acro in unique_cu_acros) {
      filtered_data <- FWRA %>%
        filter(CU_ACRO == cu_acro)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("CU_ACRO_Current", cu_acro, sep="_"))
    }

# Loop for CU_ACRO future risk
    for (cu_acro in unique_cu_acros) {
      filtered_data <- FWRA %>%
        filter(CU_ACRO == cu_acro)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("CU_ACRO_Future", cu_acro, sep="_"))
    }

# Loop for Area current risk
    for (area in unique_areas) {
      filtered_data <- FWRA %>%
        filter(Area == area)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("Area_Current", area, sep="_"))
    }

# Loop for Area future risk
    for (area in unique_areas) {
      filtered_data <- FWRA %>%
        filter(Area == area)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("Area_Future", area, sep="_"))
    }

# Load the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)

# Get the data output path
data.output.path <- "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data Output"

# Define a function for processing the data and creating CSV files
process_filtered_data <- function(data, bio_risk, file_suffix) {
  # Assume that your process_filtered_data function looks something like this:
  sorted_risks <- data %>%
    group_by(LF_Number) %>%
    summarise(Risk = mean(get(bio_risk)))
  
  # Print the sorted risks data frame to the console
  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  # Save the sorted risks data frame to a CSV file
  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_risks_", file_suffix, ".csv"))
}

# Get unique values for SMU, CU_ACRO, and Area
unique_values <- list(SMU = unique(FWRA$SMU),
                      CU_ACRO = unique(FWRA$CU_ACRO),
                      Area = unique(FWRA$Area))

# Loop for each unique value and corresponding risk
for (param in names(unique_values)) {
  for (val in unique_values[[param]]) {
    for (risk in c("Current_Bio_Risk", "Future_Bio_Risk")) {
      # Filter the data and process it
      filtered_data <- FWRA %>% filter(get(param) == val)
      process_filtered_data(data = filtered_data, bio_risk = risk, file_suffix = paste(param, risk, val, sep = "_"))
    }
  }
}

# Function to process data files and generate plots
process_data_files <- function(current_file_name, future_file_name, data_output_path) {
  
  # Read the CSV files if they are saved in previous steps
  current_data <- read.csv(file = paste0(data_output_path, "/", current_file_name))
  future_data <- read.csv(file = paste0(data_output_path, "/", future_file_name))

  # Add a new column to each dataset to indicate if it's current or future data
  current_data$Time_Period <- 'Current'
  future_data$Time_Period <- 'Future'

  # Process the data, calculate the rank, and reshape the data
  # ....

  # Create the plot and save it to a file
    plot <- ggplot(data_top, aes(x = reorder(LF_Number, Rank), y = Proportion, fill = Risk_Level)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    labs(x = NULL, y = "Proportion of Each Risk") +
    scale_fill_manual(values = c("VH" = "red3", "H" = "darkorange1", "M" = "gold1", "HPDG" = "grey30", "LPDG" = "grey70", "L" = "yellowgreen", "VL" = "forestgreen")) +
    coord_flip() +
    facet_wrap(~ Time_Period, ncol = 2, scales = "free", strip.position = "top") +
    theme(
      axis.text.y = element_text(size = 15, face = "bold", margin = margin(r = 0.1, l = 0.1, unit = "cm")),
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.title = element_blank(),  # Remove axis titles
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size = 18, face = "bold")
    )
  ggsave(filename = paste0(data_output_path, "/", "plot_", file_suffix, ".png"), plot = plot)
}

# Loop over each unique value and corresponding risk again
for (param in names(unique_values)) {
  for (val in unique_values[[param]]) {
    for (risk in c("Current_Bio_Risk", "Future_Bio_Risk")) {
      # Process data and generate plots
      process_data_files(
        current_file_name = paste("sorted_risks_", param, "Current_Bio_Risk", val, ".csv", sep = "_"),
        future_file_name = paste("sorted_risks_", param, "Future_Bio_Risk", val, ".csv", sep = "_"),
        data_output_path = data.output.path
      )
    }
  }
}


###### System Site Risk Tables######
FWRA <- read_excel(paste(data.path, "FWRA_2021_RESULTS_MASTER.xlsx", sep = "/"), sheet = 1)


FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"


####Risk Ranked Across All LFs and Spatial Scales####
# remove HPDG and LPDG from data
library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"



# Define a function to add missing columns to a data frame and fill them with 0s
add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% colnames(df)) {
      df[[column]] <- 0
    }
  }
  return(df)
}

# Define a function to process filtered data: calculate proportions, sort risks, and save to a CSV file
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  risk_summary <- filtered_data %>%
    mutate(VH = ifelse(!!sym(risk_column) %in% c("VH"), 1, 0),
           H = ifelse(!!sym(risk_column) %in% c("H"), 1, 0),
           M = ifelse(!!sym(risk_column) %in% c("M"), 1, 0),
           L = ifelse(!!sym(risk_column) %in% c("L"), 1, 0),
           VL = ifelse(!!sym(risk_column) %in% c("VL"), 1, 0), 
           LPDG = ifelse(!!sym(risk_column) %in% c("LPDG"), 1, 0),
           HPDG = ifelse(!!sym(risk_column) %in% c("HPDG"), 1, 0)) %>%
            
    group_by(LF_Number,LF_Name) %>%
    summarise(VH_total_count = sum(VH),
              H_total_count = sum(H),
              M_total_count = sum(M),
              L_total_count = sum(L),
              VL_total_count = sum(VL),
              LPDG_total_count = sum(LPDG),
              HPDG_total_count = sum(HPDG),
              total_count = n()) 
  # Sort the risk summary data frame by the highest proportion for each risk level in the order of risk levels
  sorted_risks <- risk_summary %>%
    arrange(desc(VH_total_count), desc(H_total_count), desc(M_total_count), HPDG_total_count, desc(L_total_count), desc(VL_total_count), desc(LPDG_total_count)) %>%
    select(LF_Number,LF_Name, VH_total_count, H_total_count, M_total_count, HPDG_total_count, L_total_count, VL_total_count, LPDG_total_count)

  # Print the sorted risks data frame to the console
  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  # Save the sorted risks data frame to a CSV file
  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_risks_", file_suffix, ".csv"))
}


# Loop for SYSTEM_SITE current risk
    for (system_site in unique_system_sites) {
      filtered_data <- FWRA %>%
        filter(SYSTEM_SITE == system_site)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("SYSTEM_SITE_Current", system_site, sep="_"))
    }

# Loop for SYSTEM_SITE future risk
    for (system_site in unique_system_sites) {
      filtered_data <- FWRA %>%
        filter(SYSTEM_SITE == system_site)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("SYSTEM_SITE_Future", system_site, sep="_"))
    }

#####CU and DU Data Gaps#####
FWRA <- read_excel(paste(data.path, "FWRA_2021_RESULTS_MASTER.xlsx", sep = "/"), sheet = 1)

FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"


####Risk Ranked Across All LFs and Spatial Scales####
library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"



# Define a function to add missing columns to a data frame and fill them with 0s
add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% colnames(df)) {
      df[[column]] <- 0
    }
  }
  return(df)
}

# Define a function to process filtered data: calculate proportions, sort risks, and save to a CSV file
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  risk_summary <- filtered_data %>%
    mutate(VH = ifelse(!!sym(risk_column) %in% c("VH"), 1, 0),
           H = ifelse(!!sym(risk_column) %in% c("H"), 1, 0),
           M = ifelse(!!sym(risk_column) %in% c("M"), 1, 0),
           L = ifelse(!!sym(risk_column) %in% c("L"), 1, 0),
           VL = ifelse(!!sym(risk_column) %in% c("VL"), 1, 0), 
           LPDG = ifelse(!!sym(risk_column) %in% c("LPDG"), 1, 0),
           HPDG = ifelse(!!sym(risk_column) %in% c("HPDG"), 1, 0)) %>%
            
    group_by(LF_Number,LF_Name) %>%
    summarise(VH_total_count = sum(VH),
              H_total_count = sum(H),
              M_total_count = sum(M),
              L_total_count = sum(L),
              VL_total_count = sum(VL),
              LPDG_total_count = sum(LPDG),
              HPDG_total_count = sum(HPDG),
              total_count = n()) %>% # Count the total number of rows in the filtered_data
    mutate(VH_total_prop = VH_total_count / total_count,
           H_total_prop = H_total_count / total_count,
           M_total_prop = M_total_count / total_count,
           L_total_prop = L_total_count / total_count,
           VL_total_prop = VL_total_count / total_count,
           LPDG_total_prop = LPDG_total_count / total_count,
           HPDG_total_prop = HPDG_total_count / total_count)

  # Sort the risk summary data frame by the highest proportion for each risk level in the order of risk levels
  sorted_risks <- risk_summary %>%
    arrange(desc(HPDG_total_prop), desc(VH_total_prop), desc(H_total_prop), desc(M_total_prop), desc(L_total_prop), desc(VL_total_prop), desc(LPDG_total_prop)) %>%
    select(LF_Number, LF_Name,total_count, HPDG_total_prop, HPDG_total_count,VH_total_prop, VH_total_count, H_total_prop, H_total_count, M_total_prop, M_total_count, L_total_prop, L_total_count, VL_total_prop, VL_total_count, LPDG_total_prop, LPDG_total_count)

  # Print the sorted risks data frame to the console
  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  # Save the sorted risks data frame to a CSV file
  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_data_gaps_", file_suffix, ".csv"))
}


# Get unique values for LF_Number, CU_ACRO, Area, and SYSTEM_SITE
unique_lf_numbers <- unique(FWRA$LF_Number)
unique_cu_acros <- unique(FWRA$CU_ACRO)
unique_areas <- unique(FWRA$Area)
unique_system_sites <- unique(FWRA$SYSTEM_SITE)

# Loop for CU_ACRO current risk
    for (cu_acro in unique_cu_acros) {
      filtered_data <- FWRA %>%
        filter(CU_ACRO == cu_acro)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("CU_ACRO_Current", cu_acro, sep="_"))
    }

# Loop for CU_ACRO future risk
    for (cu_acro in unique_cu_acros) {
      filtered_data <- FWRA %>%
        filter(CU_ACRO == cu_acro)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("CU_ACRO_Future", cu_acro, sep="_"))
    }

# Loop for Area current risk
    for (area in unique_areas) {
      filtered_data <- FWRA %>%
        filter(Area == area)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("Area_Current", area, sep="_"))
    }

# Loop for Area future risk
    for (area in unique_areas) {
      filtered_data <- FWRA %>%
        filter(Area == area)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("Area_Future", area, sep="_"))
    }

# Load the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Define a function that takes two arguments, the names of the current and future CSV files
process_data_files <- function(current_file_name, future_file_name) {
  # Read the CSV files if they are saved in previous steps
  current_data <- read.csv(file = paste0(data.output.path, "/", current_file_name))
  future_data <- read.csv(file = paste0(data.output.path, "/", future_file_name))

  # Add a new column to each dataset to indicate if it's current or future data
  current_data$Time_Period <- 'Current'
  future_data$Time_Period <- 'Future'

  # Combine the current and future data into one dataframe
  combined_data <- rbind(current_data, future_data)

  # Reshape the data into a long format suitable for plotting
  data_long <- combined_data %>% 
    select(LF_Number, Time_Period, VH_total_prop, H_total_prop, M_total_prop, L_total_prop, VL_total_prop, LPDG_total_prop, HPDG_total_prop) %>%
    gather(key = "Risk_Level", value = "Proportion", -LF_Number, -Time_Period)

  # Assign weights according to risk category importance
  weights <- c(VH = 7, H = 6, M = 5, HPDG = 4, L = 3, VL = 2, LPDG = 1)

  # Calculate the rank based on weighted sum of proportions
  combined_data <- combined_data %>% 
    mutate(Rank = VH_total_prop * weights['VH'] +
                  H_total_prop * weights['H'] +
                  M_total_prop * weights['M'] +
                  HPDG_total_prop * weights['HPDG'] +
                  L_total_prop * weights['L'] +
                  VL_total_prop * weights['VL'] +
                  LPDG_total_prop * weights['LPDG'])

  # Merge the ranking with the data_long dataframe
  data_long <- merge(data_long, combined_data[, c("LF_Number", "Time_Period", "Rank")], by = c("LF_Number", "Time_Period"))

  # [The rest of your data processing and plotting code goes here]

  # Return the final plot
  return(plot)
}

# Define the group names
groups <- c("Area", "CU", "SMU")

# Apply the function for each group
for (group in groups) {
  current_file_name <- paste0("sorted_risks_", group, "_Current_WVI.csv")
  future_file_name <- paste0("sorted_risks_", group, "_Future_WVI.csv")
  
  plot <- process_data_files(current_file_name, future_file_name)
  
  # Display the plot
  print(plot)
}


####SYSTEM_SITE DATA GAPS####
FWRA <- read_excel(paste(data.path, "FWRA_2021_RESULTS_MASTER.xlsx", sep = "/"), sheet = 1)


FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"


####Risk Ranked Across All LFs and Spatial Scales####
# remove HPDG and LPDG from data
library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"



# Define a function to add missing columns to a data frame and fill them with 0s
add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% colnames(df)) {
      df[[column]] <- 0
    }
  }
  return(df)
}

# Define a function to process filtered data: calculate proportions, sort risks, and save to a CSV file
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  risk_summary <- filtered_data %>%
    mutate(VH = ifelse(!!sym(risk_column) %in% c("VH"), 1, 0),
           H = ifelse(!!sym(risk_column) %in% c("H"), 1, 0),
           M = ifelse(!!sym(risk_column) %in% c("M"), 1, 0),
           L = ifelse(!!sym(risk_column) %in% c("L"), 1, 0),
           VL = ifelse(!!sym(risk_column) %in% c("VL"), 1, 0), 
           LPDG = ifelse(!!sym(risk_column) %in% c("LPDG"), 1, 0),
           HPDG = ifelse(!!sym(risk_column) %in% c("HPDG"), 1, 0)) %>%
            
    group_by(LF_Number,LF_Name) %>%
    summarise(VH_total_count = sum(VH),
              H_total_count = sum(H),
              M_total_count = sum(M),
              L_total_count = sum(L),
              VL_total_count = sum(VL),
              LPDG_total_count = sum(LPDG),
              HPDG_total_count = sum(HPDG),
              total_count = n()) 
  # Sort the risk summary data frame by the highest proportion for each risk level in the order of risk levels
  sorted_risks <- risk_summary %>%
    arrange(desc(HPDG_total_count), desc(VH_total_count), desc(H_total_count), M_total_count, desc(L_total_count), desc(VL_total_count), desc(LPDG_total_count)) %>%
    select(LF_Number,LF_Name, HPDG_total_count, VH_total_count, H_total_count, M_total_count, L_total_count, VL_total_count, LPDG_total_count)

  # Print the sorted risks data frame to the console
  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  # Save the sorted risks data frame to a CSV file
  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_data_gaps_", file_suffix, ".csv"))
}


# Loop for SYSTEM_SITE current risk
    for (system_site in unique_system_sites) {
      filtered_data <- FWRA %>%
        filter(SYSTEM_SITE == system_site)
      process_filtered_data(filtered_data, "Current_Bio_Risk", paste("SYSTEM_SITE_Current", system_site, sep="_"))
    }

# Loop for SYSTEM_SITE future risk
    for (system_site in unique_system_sites) {
      filtered_data <- FWRA %>%
        filter(SYSTEM_SITE == system_site)
      process_filtered_data(filtered_data, "Future_Bio_Risk", paste("SYSTEM_SITE_Future", system_site, sep="_"))
    }



# Define a function to get the LF_Names from sorted tables
get_LF_Names <- function(system_site, risk_period) {
  # Read the risk table for the given system site and risk period
  risk_table <- read.csv(paste0(data.output.path, "/sorted_risks_System_Site_", risk_period, "_", system_site, ".csv"))
  
  # Extract the LF_Name column and add a new column for the system site and risk period
  LF_Names <- risk_table %>%
    select(LF_Name) %>%
    mutate(System_Site = system_site, Risk_Period = risk_period)
  
  # Return the LF_Names dataframe
  return(LF_Names)
}

# Initialize an empty list to store the LF_Names dataframes
LF_Names_list <- list()

# Loop for each unique system site
for (system_site in unique_system_sites) {
  # Loop for each risk period
  for (risk_period in c("Current", "Future")) {
    # Get the LF_Names dataframe for the system site and risk period and add it to the list
    LF_Names_list[[paste(system_site, risk_period, sep = "_")]] <- get_LF_Names(system_site, risk_period)
  }
}

## Combine all the LF_Names dataframes into one dataframe
combined_LF_Names <- bind_rows(LF_Names_list)

# Spread the combined dataframe to have LF_Names in a column for each system site side by side
spread_combined_LF_Names <- combined_LF_Names %>%
  unite("System_Site_Risk_Period", System_Site, Risk_Period, remove = FALSE) %>%
  group_by(System_Site_Risk_Period) %>% 
  mutate(row_number = row_number()) %>%
  spread(key = System_Site_Risk_Period, value = LF_Name)

# Remove NAs and shift up non-NA values
spread_combined_LF_Names[] <- lapply(spread_combined_LF_Names, function(x) {
   x <- na.omit(x)
   length(x) <- nrow(spread_combined_LF_Names)
   x
})

# Write the spread combined dataframe to a CSV file
write.csv(spread_combined_LF_Names, file = paste0(data.output.path, "/spread_combined_LF_Names.csv"))

# Print the spread combined dataframe to the console
print(spread_combined_LF_Names)


####breakdown of list with life stage in mind########
FWRA <- read_excel(paste(data.path, "FWRA_2021_RESULTS_MASTER.xlsx", sep = "/"), sheet = 1)


FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"


#add column SMU and replicate "SMU" for all rows
FWRA$SMU <- "WVI"

library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"

# Define a function to process filtered data: calculate proportions, sort risks, and save to a CSV file
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  risk_summary <- filtered_data %>%
    mutate(VH = ifelse(!!sym(risk_column) %in% c("VH"), 1, 0),
           H = ifelse(!!sym(risk_column) %in% c("H"), 1, 0),
           M = ifelse(!!sym(risk_column) %in% c("M"), 1, 0),
           L = ifelse(!!sym(risk_column) %in% c("L"), 1, 0),
           VL = ifelse(!!sym(risk_column) %in% c("VL"), 1, 0), 
           LPDG = ifelse(!!sym(risk_column) %in% c("LPDG"), 1, 0),
           HPDG = ifelse(!!sym(risk_column) %in% c("HPDG"), 1, 0)) %>%
            
    group_by(Stage, LF_Number,LF_Name) %>%
    summarise(VH_total_count = sum(VH),
              H_total_count = sum(H),
              M_total_count = sum(M),
              L_total_count = sum(L),
              VL_total_count = sum(VL),
              LPDG_total_count = sum(LPDG),
              HPDG_total_count = sum(HPDG),
              total_count = n()) %>% 
    mutate(VH_total_prop = VH_total_count / total_count,
           H_total_prop = H_total_count / total_count,
           M_total_prop = M_total_count / total_count,
           L_total_prop = L_total_count / total_count,
           VL_total_prop = VL_total_count / total_count,
           LPDG_total_prop = LPDG_total_count / total_count,
           HPDG_total_prop = HPDG_total_count / total_count)

  sorted_risks <- risk_summary %>%
    arrange(desc(VH_total_prop), desc(H_total_prop), desc(M_total_prop),desc(HPDG_total_prop), desc(L_total_prop), desc(VL_total_prop), desc(LPDG_total_prop)) %>%
    select(Stage, LF_Number, LF_Name,total_count,VH_total_prop, VH_total_count, H_total_prop, H_total_count, M_total_prop, M_total_count, HPDG_total_prop, HPDG_total_count,L_total_prop, L_total_count, VL_total_prop, VL_total_count, LPDG_total_prop, LPDG_total_count)

  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_risks_", file_suffix, ".csv"))
}



# Get unique values for LF_Number, CU_ACRO, Area, SYSTEM_SITE, and Stage
colnames(FWRA)
unique_lf_numbers <- unique(FWRA$LF_Number)
unique_cu_acros <- unique(FWRA$CU_ACRO)
unique_areas <- unique(FWRA$Area)
unique_system_sites <- unique(FWRA$SYSTEM_SITE)
unique_stages <- unique(FWRA$Stage)
unique_SMU <- unique(FWRA$SMU)

# Loop for SMU current risk
for (smu in unique_SMU) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(SMU == smu & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("SMU_Current", smu, "_Stage", stage, sep="_"))
  }
}

# Loop for SMU future risk
for (smu in unique_SMU) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(SMU == smu & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("SMU_Future", smu, "_Stage", stage, sep="_"))
  }
}

# Loop for CU_ACRO current risk
for (cu_acro in unique_cu_acros) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(CU_ACRO == cu_acro & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("CU_ACRO_Current", cu_acro, "_Stage", stage, sep="_"))
  }
}

# Loop for CU_ACRO future risk
for (cu_acro in unique_cu_acros) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(CU_ACRO == cu_acro & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("CU_ACRO_Future", cu_acro, "_Stage", stage, sep="_"))
  }
}

# Loop for Area current risk
for (area in unique_areas) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(Area == area & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("Area_Current", area, "_Stage", stage, sep="_"))
  }
}

# Loop for Area future risk
for (area in unique_areas) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(Area == area & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("Area_Future", area, "_Stage", stage, sep="_"))
  }
}

# Loop for SYSTEM_SITE current risk
for (system_site in unique_system_sites) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(SYSTEM_SITE == system_site & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("SYSTEM_SITE_Current", system_site, "_Stage", stage, sep="_"))
  }
}

# Loop for SYSTEM_SITE future risk
for (system_site in unique_system_sites) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(SYSTEM_SITE == system_site & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("SYSTEM_SITE_Future", system_site, "_Stage", stage, sep="_"))
  }
}


#######Tla-lo-qui-aht First Nations#######



FWRA <- read_excel(paste(data.path, "FWRA_2021_RESULTS_MASTER.xlsx", sep = "/"), sheet = 1)


FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")

#add "LF" before LF number
FWRA$LF <- paste("LF", FWRA$LF_Number, sep = "")

FWRA$Current_Bio_Risk <- as.character(FWRA$Current_Bio_Risk)
FWRA$Future_Bio_Risk <- as.character(FWRA$Future_Bio_Risk)

#change all numeric values to character
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="1"]<-"VL"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="2"]<-"L"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="3"]<-"M"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="4"]<-"H"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="5"]<-"VH"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="0"]<-"LPDG"
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk=="-1"]<-"HPDG"


FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="1"]<-"VL"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="2"]<-"L"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="3"]<-"M"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="4"]<-"H"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="5"]<-"VH"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="0"]<-"LPDG"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk=="-1"]<-"HPDG"

library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"

# Define a function to process filtered data: calculate proportions, sort risks, and save to a CSV file
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  risk_summary <- filtered_data %>%
    mutate(VH = ifelse(!!sym(risk_column) %in% c("VH"), 1, 0),
           H = ifelse(!!sym(risk_column) %in% c("H"), 1, 0),
           M = ifelse(!!sym(risk_column) %in% c("M"), 1, 0),
           L = ifelse(!!sym(risk_column) %in% c("L"), 1, 0),
           VL = ifelse(!!sym(risk_column) %in% c("VL"), 1, 0), 
           LPDG = ifelse(!!sym(risk_column) %in% c("LPDG"), 1, 0),
           HPDG = ifelse(!!sym(risk_column) %in% c("HPDG"), 1, 0)) %>%
            
    group_by(Stage, LF_Number,LF_Name) %>%
    summarise(VH_total_count = sum(VH),
              H_total_count = sum(H),
              M_total_count = sum(M),
              L_total_count = sum(L),
              VL_total_count = sum(VL),
              LPDG_total_count = sum(LPDG),
              HPDG_total_count = sum(HPDG),
              total_count = n()) %>% 
    mutate(VH_total_prop = VH_total_count / total_count,
           H_total_prop = H_total_count / total_count,
           M_total_prop = M_total_count / total_count,
           L_total_prop = L_total_count / total_count,
           VL_total_prop = VL_total_count / total_count,
           LPDG_total_prop = LPDG_total_count / total_count,
           HPDG_total_prop = HPDG_total_count / total_count)

  sorted_risks <- risk_summary %>%
    arrange(desc(VH_total_prop), desc(H_total_prop), desc(M_total_prop),desc(HPDG_total_prop), desc(L_total_prop), desc(VL_total_prop), desc(LPDG_total_prop)) %>%
    select(Stage, LF_Number, LF_Name,total_count,VH_total_prop, VH_total_count, H_total_prop, H_total_count, M_total_prop, M_total_count, HPDG_total_prop, HPDG_total_count,L_total_prop, L_total_count, VL_total_prop, VL_total_count, LPDG_total_prop, LPDG_total_count)

  cat("\nSorted Risks for", file_suffix, ":\n")
  print(sorted_risks)

  write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_risks_", file_suffix, ".csv"))
}

# Get unique values for LF_Number, CU_ACRO, Area, SYSTEM_SITE, and Stage
unique_lf_numbers <- unique(FWRA$LF_Number)
unique_cu_acros <- unique(FWRA$CU_ACRO)
unique_areas <- unique(FWRA$Area)
unique_system_sites <- unique(FWRA$SYSTEM_SITE)
unique_stages <- unique(FWRA$Stage)

# Loop for CU_ACRO current risk
for (cu_acro in unique_cu_acros) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(CU_ACRO == cu_acro & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("CU_ACRO_Current", cu_acro, "_Stage", stage, sep="_"))
  }
}

# Loop for CU_ACRO future risk
for (cu_acro in unique_cu_acros) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(CU_ACRO == cu_acro & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("CU_ACRO_Future", cu_acro, "_Stage", stage, sep="_"))
  }
}

# Loop for Area current risk
for (area in unique_areas) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(Area == area & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("Area_Current", area, "_Stage", stage, sep="_"))
  }
}

# Loop for Area future risk
for (area in unique_areas) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(Area == area & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("Area_Future", area, "_Stage", stage, sep="_"))
  }
}

# Loop for SYSTEM_SITE current risk
for (system_site in unique_system_sites) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(SYSTEM_SITE == system_site & Stage == stage)
    process_filtered_data(filtered_data, "Current_Bio_Risk", paste("SYSTEM_SITE_Current", system_site, "_Stage", stage, sep="_"))
  }
}

# Loop for SYSTEM_SITE future risk
for (system_site in unique_system_sites) {
  for (stage in unique_stages) {
    filtered_data <- FWRA %>%
      filter(SYSTEM_SITE == system_site & Stage == stage)
    process_filtered_data(filtered_data, "Future_Bio_Risk", paste("SYSTEM_SITE_Future", system_site, "_Stage", stage, sep="_"))
  }
}
