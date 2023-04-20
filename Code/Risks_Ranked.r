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

#################### Risk Table ####################
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
FWRA <- subset(FWRA, Current_Bio_Risk != "HPDG" & Current_Bio_Risk != "LPDG" & Future_Bio_Risk != "HPDG" & Future_Bio_Risk != "LPDG")
library(tidyr)
library(dplyr)

# Add 0s to missing values in current risk column and future risk column
FWRA$Current_Bio_Risk[FWRA$Current_Bio_Risk==""]<-"0"
FWRA$Future_Bio_Risk[FWRA$Future_Bio_Risk==""]<-"0"



# Add the add_missing_columns function
add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% colnames(df)) {
      df[[column]] <- 0
    }
  }
  return(df)
}
process_filtered_data <- function(filtered_data, risk_column, file_suffix) {
  total_risk_proportions <- filtered_data %>%
    count(LF_Number, !!sym(risk_column)) %>%
    group_by(LF_Number) %>%
    mutate(Prop_Total = n / sum(n)) %>%
    ungroup()

  risk_summary <- total_risk_proportions %>%
    group_by(LF_Number, !!sym(risk_column)) %>%
    summarise(total_prop = sum(Prop_Total), total_count = sum(n)) %>%
    pivot_wider(names_from = !!sym(risk_column), values_from = c(total_prop, total_count), values_fill = 0)

  if (nrow(risk_summary) > 0) {
    risk_summary <- add_missing_columns(risk_summary, c("VH_total_prop", "H_total_prop", "M_total_prop", "L_total_prop", "VL_total_prop", "VH_total_count", "H_total_count", "M_total_count", "L_total_count", "VL_total_count"))

    # Ungroup the dataframe before arranging
    risk_summary <- risk_summary %>% ungroup()

    risk_summary_before_arrange <- risk_summary %>%
  ungroup()

print(risk_summary_before_arrange)

    sorted_risks <- risk_summary %>%
      arrange(desc(VH_total_prop), desc(H_total_prop), desc(M_total_prop), desc(L_total_prop), desc(VL_total_prop), desc(LF_Number))

    # Save the results to a CSV file
    write.csv(sorted_risks, file = paste0(data.output.path, "/sorted_risks_", file_suffix, ".csv"))
    return(sorted_risks) # Return the sorted_risks dataframe
  } else {
    cat("No data for", file_suffix, "\n")
    return(NULL)
  }
  manually_sorted_risks <- risk_summary_before_arrange %>%
  arrange(desc(VH_total_prop), desc(H_total_prop), desc(M_total_prop), desc(L_total_prop), desc(VL_total_prop))

print(manually_sorted_risks)
}
for (area in unique_areas) {
  filtered_data <- FWRA %>%
    filter(Area == area)
  print(paste("Area:", area))
  print(filtered_data)  # Add this line to print filtered_data
  sorted_risks <- process_filtered_data(filtered_data, "Future_Bio_Risk", paste("Area_Future", area, sep="_"))
  if (!is.null(sorted_risks)) {
    print(sorted_risks)
  }
}