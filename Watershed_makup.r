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

#################### Risk Table ####################
FWRA <- read_excel("/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx", sheet = 1)
warnings()

head(FWRA)
colnames(FWRA)
FWRA <- subset(FWRA, LF_Number != "23" & LF_Number != "24")


# Create a sample data frame
df <- FWRA

# Set the row below which you want to start separating rows
start_row <- 1541

# Create a new data frame with the separated rows
df_new <- rbind(df[1:start_row, ], df[(start_row+1):(nrow(df)-69), ], df[(nrow(df)-68):nrow(df), ])

# Check the new data frame
head(df_new)
tail(df_new)

#save as csv
write.csv(df_new, file = "FWRA_clean.csv", row.names = FALSE)


''

# Load required libraries
library(readxl)
library(dplyr)
library(writexl)

# Read first sheet of both Excel files
file1 <- "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER.xlsx"
file2 <- "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER_Chane_Implemented.xlsx"
sheet1 <- read_excel(file1, sheet = 1)
sheet2 <- read_excel(file2, sheet = 1)
# Function to compare two dataframes and return differences by row with cell indicators
compare_dataframes_by_row <- function(df1, df2) {
  if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2) || !identical(colnames(df1), colnames(df2))) {
    stop("The dimensions or column names of the two data frames do not match.")
  }

  # Compare rows and return a logical vector of differences
  row_differences <- apply(df1 == df2, 1, all)
  diff_indices <- which(!row_differences)

  # Find the differing cells
  differing_cells <- t(apply(df1[diff_indices, ] != df2[diff_indices, ], 1, function(x) {
    paste(colnames(df1)[x], collapse = ", ")
  }))

  # Create a new data frame with the differing rows from both files
  df1_diff <- df1[diff_indices, ]
  df2_diff <- df2[diff_indices, ]

  # Add an identifier column and a column with the differing cells
  df1_diff$Origin <- "Original"
  df2_diff$Origin <- "Updated"
  df1_diff$Differing_Cells <- differing_cells
  df2_diff$Differing_Cells <- differing_cells

  combined_df <- bind_rows(df1_diff, df2_diff)

  # Add a row index column
  combined_df$Row <- rep(diff_indices, each = 2)

  combined_df
}

# Compare and store differences in a new dataframe
differences_by_row <- compare_dataframes_by_row(sheet1, sheet2)

# Write differences to a new Excel file
write_xlsx(differences_by_row, "/Users/critty/Desktop/Base/GitHub/FWRA_2022_Analysis/Data/differences_output_by_row.xlsx")
