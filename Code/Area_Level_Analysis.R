######Intro#######
#This script is for the FWRA 2022 analysis

#GUIDE#
#1. Load libraries or install libraries
#--------------any libraries needed are loaded and displayed below--------------
#install packages below if needed
#install.packages("boot","mass","plyr","dplyr","ggplot2","tibble","reshape2","epitools","readxl","tidyverse","arsenal","patchwork","palmerpenguins","viridis","ggthemes","ggpubr","dplyr","zoo")

#load packages below
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
library(tidyverse)
library(patchwork)
library(palmerpenguins)
library(viridis)
library(ggthemes)
library(dplyr)
library(zoo)

#plot dpace
#setup plot call
library(httpgd)
hgd()
hgd_browse()

#2. Source and set themes
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")
theme_set(theme_Publication())

#3. Set working directory
wd <- print(getwd)  # working directory
wd <- "/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis"

#4. Create folders
#--------------make project folders and folder paths----------------------------
# create a vector of folder names
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

#5. Load data
#---------------------Below, we upload and clean the data----------
data.path <- paste(wd, "/", "Data", sep = "")

# time to upload the datas
FWRA <- read_excel("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/FWRA_2021_RESULTS_MASTER_09.12.2022.xlsx", sheet = 1)

#6. Clean data
#Subset data that is not needed

FWRA <- subset(FWRA, LF != 23 & LF != 24)

#6. Add columns

#Add Area column
FWRA <- FWRA %>%
  mutate(Area = case_when(
    endsWith(W, "Toquaht") ~ "Area 23",
    endsWith(W, "Nahmint") ~ "Area 23",
    endsWith(W, "Sarita") ~ "Area 23",
    endsWith(W, "Somass") ~ "Area 23",
    endsWith(W, "Megin") ~ "Area 24",
    endsWith(W, "Moyeha") ~ "Area 24",
    endsWith(W, "Cypre") ~ "Area 24",
    endsWith(W, "Bedwell") ~ "Area 24",
    endsWith(W, "Tranquil") ~ "Area 24",
    endsWith(W, "Lower Kennedy") ~ "Area 24",
    endsWith(W, "Upper Kennedy") ~ "Area 24",
    endsWith(W, "Sand") ~ "Area 24",
    endsWith(W, "Clayoquot") ~ "Area 24",
    endsWith(W, "Muriel") ~ "Area 24",
    endsWith(W, "Tahsis") ~ "Area 25",
    endsWith(W, "Leiner") ~ "Area 25",
    endsWith(W, "Tsowwin") ~ "Area 25",
    endsWith(W, "Sucwoa") ~ "Area 25",
    endsWith(W, "Canton") ~ "Area 25",
    endsWith(W, "Conuma") ~ "Area 25",
    endsWith(W, "Artlish") ~ "Area 26",
    endsWith(W, "Kaouk") ~ "Area 26",
    ))

#7. Create new dataframes
#convert numeric to character
FWRA_Character <- FWRA

#convert numeric to character for CR
FWRA_Character$CR[FWRA_Character$CR=="-1"]<-"HPDG"
FWRA_Character$CR[FWRA_Character$CR=="0"]<-"LPDG"
FWRA_Character$CR[FWRA_Character$CR=="1"]<-"VL"
FWRA_Character$CR[FWRA_Character$CR=="2"]<-"L"
FWRA_Character$CR[FWRA_Character$CR=="3"]<-"M"
FWRA_Character$CR[FWRA_Character$CR=="4"]<-"H"
FWRA_Character$CR[FWRA_Character$CR=="5"]<-"VH"

#convert numeric to character for FR
FWRA_Character$FR[FWRA_Character$FR=="-1"]<-"HPDG"
FWRA_Character$FR[FWRA_Character$FR=="0"]<-"LPDG"
FWRA_Character$FR[FWRA_Character$FR=="1"]<-"VL"
FWRA_Character$FR[FWRA_Character$FR=="2"]<-"L"
FWRA_Character$FR[FWRA_Character$FR=="3"]<-"M"
FWRA_Character$FR[FWRA_Character$FR=="4"]<-"H"
FWRA_Character$FR[FWRA_Character$FR=="5"]<-"VH"

# Load dplyr library
library(dplyr)

# Select columns CR and FR
C <- FWRA_Character %>%
  select(CR, FR, Area)

# Create a vector of column names
cols <- c("CR", "FR")

# Create an empty list to store the data frames
df_list <- list()

# Loop through the vector of column names
for (col in cols) {
  
  # Group the data frame by the 'Area' column
  C_grouped <- C %>% group_by(Area)

  # Count the number of times each value occurs in the column by group
  C_counted <- C_grouped %>% count(!!as.name(col))

  # Rename the 'n' column to 'count'
  C_renamed <- C_counted %>% rename(count = n)

  # Sort the data frame by the 'group' column
  C_sorted <- C_renamed %>% arrange(Area)

  # Factor the data
  C_factored <- C_sorted
  C_factored[[col]] <- factor(C_factored[[col]], levels = c("LPDG", "HPDG", "VL", "L", "M", "H", "VH"))

  # Append the data frame to the list
  df_list[[col]] <- C_factored

}

 CR <- df_list["CR"]
 FR <- df_list["FR"]

#Select only HPDG and LPDG
CDG <- CR[[1]] %>% filter(CR == "HPDG" | CR == "LPDG")
FDG <- FR[[1]] %>% filter(FR == "HPDG" | FR == "LPDG")

#Remove HPDG and LPDG and put into new dataframe
CR <- CR[[1]] %>% filter(CR != "HPDG" & CR != "LPDG")
FR <- FR[[1]] %>% filter(FR != "HPDG" & FR != "LPDG")

# Create a list of data frames
data_frames <- list(CR, FR, CDG, FDG)

#vector for areas
Areas <- unique(FWRA$Area)

#factor data frames by Area
data_frames <- lapply(data_frames, function(df) {
  df$Area <- factor(df$Area, levels = Areas)
  return(df)
})

#replicate the sum of the count based on each area into a new column based on the number of rows in each area
# Iterate over the data frames
df_names <- c("CR", "FR", "CDG", "FDG")

for (i in seq_along(data_frames)) {
  
  # Summarize the counts by area
  area_counts <- data_frames[[i]] %>% 
    group_by(Area) %>% 
    summarize(count = sum(count))
  
  # Join the counts to the original data frame
  data_frames[[i]] <- data_frames[[i]] %>% 
    left_join(area_counts, by = "Area")
  
  # Save the updated data frame
  assign(x = df_names[i], value = data_frames[[i]])
}

#8. Make proportion column

# Use lapply to apply a function to each element of the list
data_frames <- lapply(data_frames, function(df) {
  library(dplyr)
  # Calculate the proportion for each dataframe and sum only the rows in Area 23
  df$Proportion <- (df$count.x / df$count.y)
  # Remove the 'count' column
  df$count <- NULL
#   Return the modified data frame
  return(df)
})

#use lapply to remove the count.x and count.y columns
#data_frames <- lapply(data_frames, function(df) {
 # df$count.x <- NULL
  #df$count.y <- NULL
  #return(df)
#})


# take dfs from list and save as new dfs
CR <- data_frames[[1]]
FR <- data_frames[[2]]
CDG <- data_frames[[3]]
FDG <- data_frames[[4]]

#colnames
colnames(CR)

#rename columns
names(CR) <- c("Area", "Rating", "Count","Total","Proportion")
names(FR) <- c("Area", "Rating", "Count","Total","Proportion")
names(CDG) <- c("Area", "Rating", "Count","Total","Proportion")
names(FDG) <- c("Area", "Rating", "Count","Total","Proportion")

#add time column
CR$Time <- rep("C", length(CR$Rating))
FR$Time <- rep("F", length(FR$Rating)) 
CR_FR<- rbind(CR,FR)

#add time column
CDG$Time <- rep("C", length(CDG$Rating))
FDG$Time <- rep("F", length(FDG$Rating))
CDG_FDG<- rbind(CDG,FDG)

#check for NA
any(is.na(CR_FR))
any(is.na(CR_FR))

#remove rows with NA
CR_FR <- CR_FR[complete.cases(CR_FR), ] 
CDG_FDG <- CDG_FDG[complete.cases(CDG_FDG), ]

#9. Plot

#factor High to Low
CR_FR$Rating <- factor(CR_FR$Rating, levels = c("VH", "H", "M", "L", "VL"))
CDG_FDG$Rating <- factor(CDG_FDG$Rating, levels = c("HPDG", "LPDG"))

#rename dataframes
Current_Risk_Future_Risk_All_Areas <- CR_FR
Current_DG_Future_DG_All_Areas <- CDG_FDG

#plot for percentage of current risk and future risk
ggplot(Current_Risk_Future_Risk_All_Areas, aes(x= Time, y=Proportion, fill = Rating))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Area)+
      labs(x = "Rating Period", y = "Percentage") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M"= "gold1","H" = "darkorange1", "VH" = "red3",  "LPDG" = "grey70", "HPDG" = "grey30"))+
      scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)  

    # print the plot
    print(plot)

    # save the plot
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", "Percentage_Current_Risk_Future_Risk_All_Areas","_Risks.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)

#plot for precentage of current DG and future DG
ggplot(Current_DG_Future_DG_All_Areas, aes(x= Time, y=Proportion, fill = Rating))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Area)+
      labs(x = "Rating Period", y = "Percentage") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M"= "gold1","H" = "darkorange1", "VH" = "red3",  "LPDG" = "grey70", "HPDG" = "grey30"))+
      scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)  

    # print the plot
    print(plot)

    # save the plot
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", "Percentage_Current_DG_Future_DG_All_Areas","_Risks.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)

   #plot count for current risk and future risk
ggplot(Current_Risk_Future_Risk_All_Areas, aes(x= Time, y= Count, fill = Rating))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Area)+
      labs(x = "Rating Period", y = "Count") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M"= "gold1","H" = "darkorange1", "VH" = "red3",  "LPDG" = "grey70", "HPDG" = "grey30"))+
      scale_y_continuous(limits = c(0,300), breaks=seq(0,300,50))  
    # print the plot
    print(plot)

        # save the plot
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", "Count_of_Current_Risk_Future_Risk_All_Areas","_Risks.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)      

   #plot count for current DG and future DG
ggplot(Current_DG_Future_DG_All_Areas, aes(x= Time, y= Count, fill = Rating))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Area)+
      labs(x = "Rating Period", y = "Count") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M"= "gold1","H" = "darkorange1", "VH" = "red3",  "LPDG" = "grey70", "HPDG" = "grey30"))+
      scale_y_continuous(limits = c(0,425), breaks=seq(0,425,50))  
    # print the plot
    print(plot)

        # save the plot
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", "Count_Current_DG_Future_DG_All_Areas","_Risks.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)        

#10. Export to csv
write.csv(CR_FR, file = "/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/CR_FR.csv", row.names = FALSE)
write.csv(CDG_FDG, file = "/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Data/CDG_FDG.csv", row.names = FALSE)



#####Do the same steps above but group by waterbody#####
# Select columns CR and FR
C <- FWRA_Character %>%
  select(CR, FR, W, Area)


# Create a vector of column names
cols <- c("CR", "FR")

# Create an empty list to store the data frames
df_list <- list()

# Loop through the vector of column names
for (col in cols) {
  
  # Group the data frame by the 'W' and 'Area' column
  C_grouped <- C %>% group_by(W, Area)

  # Count the number of times each value occurs in the column by group
  C_counted <- C_grouped %>% count(!!as.name(col))

  # Rename the 'n' column to 'count'
  C_renamed <- C_counted %>% rename(count = n)

  # Sort the data frame by the 'group' column
  C_sorted <- C_renamed %>% arrange(Area)

  # Factor the data
  C_factored <- C_sorted
  C_factored[[col]] <- factor(C_factored[[col]], levels = c("LPDG", "HPDG", "VL", "L", "M", "H", "VH"))

  # Append the data frame to the list
  df_list[[col]] <- C_factored

}

 CR <- df_list["CR"]
 FR <- df_list["FR"]

#Select only HPDG and LPDG
CDG <- CR[[1]] %>% filter(CR == "HPDG" | CR == "LPDG")
FDG <- FR[[1]] %>% filter(FR == "HPDG" | FR == "LPDG")

#Remove HPDG and LPDG and put into new dataframe
CR <- CR[[1]] %>% filter(CR != "HPDG" & CR != "LPDG")
FR <- FR[[1]] %>% filter(FR != "HPDG" & FR != "LPDG")

# Create a list of data frames
data_frames <- list(CR, FR, CDG, FDG)

#vector for areas
Areas <- unique(FWRA$Area)

#factor data frames by Area
data_frames <- lapply(data_frames, function(df) {
  df$Area <- factor(df$Area, levels = Areas)
  return(df)
})

#replicate the sum of the count based on each area into a new column based on the number of rows in each area
# Iterate over the data frames
df_names <- c("CR", "FR", "CDG", "FDG")

for (i in seq_along(data_frames)) {
  
  # Summarize the counts by area
  area_counts <- data_frames[[i]] %>% 
    group_by(W) %>% 
    summarize(count = sum(count))
  
  # Join the counts to the original data frame
  data_frames[[i]] <- data_frames[[i]] %>% 
    left_join(area_counts, by = "W")
  
  # Save the updated data frame
  assign(x = df_names[i], value = data_frames[[i]])
}

#8. Make proportion column

# Use lapply to apply a function to each element of the list
data_frames <- lapply(data_frames, function(df) {
  library(dplyr)
  # Calculate the proportion for each dataframe and sum only the rows in Area 23
  df$Proportion <- (df$count.x / df$count.y)
  # Remove the 'count' column
  df$count <- NULL
  # Return the modified data frame
  return(df)
})

#use lapply to remove the count.x and count.y columns
#data_frames <- lapply(data_frames, function(df) {
 # df$count.x <- NULL
  #df$count.y <- NULL
  #return(df)
#})


# take dfs from list and save as new dfs
CR <- data_frames[[1]]
FR <- data_frames[[2]]
CDG <- data_frames[[3]]
FDG <- data_frames[[4]]

#rename columns
names(CR) <- c("Watershed","Area", "Rating", "Count","Total","Proportion")
names(FR) <- c("Watershed","Area", "Rating", "Count","Total","Proportion")
names(CDG) <- c("Watershed","Area", "Rating", "Count","Total","Proportion")
names(FDG) <- c("Watershed","Area", "Rating", "Count","Total","Proportion")


#add time column
CR$Time <- rep("C", length(CR$Rating))
FR$Time <- rep("F", length(FR$Rating)) 
CR_FR<- rbind(CR,FR)

#add time column
CDG$Time <- rep("C", length(CDG$Rating))
FDG$Time <- rep("F", length(FDG$Rating))
CDG_FDG<- rbind(CDG,FDG)

#determine if there are any NA values
any(is.na(CR_FR))
any(is.na(CDG_FDG))

#remove rows with NA
CR_FR <- CR_FR[complete.cases(CR_FR), ] 
CDG_FDG <- CDG_FDG[complete.cases(CDG_FDG), ]

#9. Plot for each area
#factor High to Low
CR_FR$Risk <- factor(CR_FR$Rating, levels = c("VH", "H", "M", "L", "VL"))
CDG_FDG$Risk <- factor(CDG_FDG$Rating, levels = c("HPDG", "LPDG"))

#rename dataframes
Current_Risk_Future_Risk_All_Areas <- CR_FR
Current_DG_Future_DG_All_Areas <- CDG_FDG

#plot for CR and FR
Area_names <- unique(Current_Risk_Future_Risk_All_Areas$Area)

# iterate over each area in Area_names
  for (area in Area_names) {
    # filter the data frame to only include data for the current area
    df_area <- filter(Current_Risk_Future_Risk_All_Areas, Area == area)

    # create a plot for the filtered data frame
    plot <- ggplot(df_area, aes(x= Time, y= Proportion, fill = Risk))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Watershed)+
      labs(x = "Rating Period", y = "Count") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M" = "gold1", "H" = "darkorange1", "VH" = "red3", "HPDG" = "grey70", "LPDG" = "grey30"))+
       scale_y_continuous(limits = c(0,1.01), breaks=seq(0,1.01,.25), labels = scales::percent)  


    #save current plot
    ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", area,"Percentage_Risks.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)
  }

#plot for CDG and FDG
Area_names <- unique(Current_DG_Future_DG_All_Areas$Area)

#iterate over each area in Area_names
  for (area in Area_names) {
    # filter the data frame to only include data for the current area
    df_area <- filter(Current_DG_Future_DG_All_Areas, Area == area)

    # create a plot for the filtered data frame
    plot <- ggplot(df_area, aes(x= Time, y= Proportion, fill = Risk))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Watershed)+
      labs(x = "Rating Period", y = "Count") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M" = "gold1", "H" = "darkorange1", "VH" = "red3", "HPDG" = "grey70", "LPDG" = "grey30"))+
       scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)  


    #save current plot
    ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", area,"Percentage_DGs.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)
  }

#######Plot for counts by areas

#plot for CR and FR
Area_names <- unique(Current_Risk_Future_Risk_All_Areas$Area)

# iterate over each area in Area_names
  for (area in Area_names) {
    # filter the data frame to only include data for the current area
    df_area <- filter(Current_Risk_Future_Risk_All_Areas, Area == area)

    # create a plot for the filtered data frame
    plot <- ggplot(df_area, aes(x= Time, y= Count, fill = Risk))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Watershed)+
      labs(x = "Rating Period", y = "Count") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M" = "gold1", "H" = "darkorange1", "VH" = "red3", "HPDG" = "grey70", "LPDG" = "grey30"))+
      scale_y_continuous(limits = c(0,50), breaks=seq(0,50,5))


    #save current plot
    ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", area,"_Count_of_Risks.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)
  }

#plot for CDG and FDG
Area_names <- unique(Current_DG_Future_DG_All_Areas$Area)

#iterate over each area in Area_names
  for (area in Area_names) {
    # filter the data frame to only include data for the current area
    df_area <- filter(Current_DG_Future_DG_All_Areas, Area == area)

    # create a plot for the filtered data frame
    plot <- ggplot(df_area, aes(x= Time, y= Count, fill = Risk))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Watershed)+
      labs(x = "Rating Period", y = "Count") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M" = "gold1", "H" = "darkorange1", "VH" = "red3", "HPDG" = "grey70", "LPDG" = "grey30"))+
            scale_y_continuous(limits = c(0,50), breaks=seq(0,50,5))

    #save current plot
    ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/All/", area,"_DGs.jpeg"),
           device = "jpeg",
           width = 30,
           height = 30,
           units = "cm",
           dpi = 300)
  }

#summary table for CR and FR
#Add time period row to CR
CR <- CR %>%
  mutate(Time = "Current")
 colnames(CR) 
#Add time period row to FR
FR <- FR %>%
  mutate(Time = "Future")
#RBIND CR and FR
Current_Risk_Future_Risk_All_Areas <- rbind(CR, FR)

colnames(Current_Risk_Future_Risk_All_Areas)

Risk_Table <- Current_Risk_Future_Risk_All_Areas %>%
  group_by(Area, Rating, Time) %>%
  summarize(sums = sum(Count))

data.frame(Risk_Table)

Table <- Risk_Table %>%
  group_by(Area, Rating, Time) %>%
  summarize(sums = sum(sums))
  
#plot table clusteered by area
Table <- data.frame(Table)
#plot table clusteered by area
Table %>% as_tibble() %>% print(n=40)

colnames(Table)
Table$sums <- as.numeric(Table$sums)



#make stacked bar chart for each area that shows the count of each risk
#label bars with values
ggplot(Table, aes(x= Time, y=sums, fill = Rating))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Area)+
      geom_text(aes(label=sums), position=position_stack(vjust=0.5), size=3)+
      labs(x = "Rating Period", y = "Percentage") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("VL" = "forestgreen", "L" = "yellowgreen", "M"= "gold1","H" = "darkorange1", "VH" = "red3"))+
      scale_y_continuous(limits = c(0,300), breaks=seq(0,300,50))
      #fix y axis values not showing up


#summary table for CDG and FDG
#Add time period row to CR
CDG <- CDG %>%
  mutate(Time = "Current")

#Add time period row to FR
FDG <- FDG %>%
  mutate(Time = "Future")

#RBIND CDG and FDG
Current_DG_Future_DG_All_Areas <- rbind(CDG, FDG)


DG_Table <- Current_DG_Future_DG_All_Areas %>%
  group_by(Area, Rating, Time) %>%
  summarize(sums = sum(Count))
DG_Table <- data.frame(DG_Table)

Risk_Table %>% as_tibble() %>% print(n=40)

DG_Table$sums <- as.numeric(DG_Table$sums)

library(ggplot2)
#make stacked bar chart for each area that shows the count of each risk
ggplot(DG_Table, aes(x= Time, y=sums, fill = Rating))+
      geom_bar(position="stack", stat="identity")+ facet_grid(~ Area)+
      geom_text(aes(label=sums), position=position_stack(vjust=0.5), size=3)+
      labs(x = "Rating Period", y = "Percentage") +
      theme_Publication()+ 
      theme(axis.text=element_text(size=14),
            axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
            axis.text.y=element_text(color="black"))+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("LPDG" = "grey70", "HPDG" = "grey30"))+
      scale_y_continuous(limits = c(0,500), breaks=seq(0,500,50))



#Combined Current + Future
Area24_combo<- Area24_combo %>%                               # Specify data frame
  group_by(Rating) %>%                         # Specify group indicator
  summarise_at(vars(n),              # Specify column
               list(Count = sum))

Area24_combo$Denominator <- c(rep(1360,7))
Area24_combo$Proportion <- Area24_combo$Count/Area24_combo$Denominator


library(vtable)
library(RColorBrewer)
library(dplyr)
library(ggplot2)


myData <- matrix(c(2,2,3,3,3,1,2,2,3,3,1,1,2,2,3,1,1,2,2,2,1,1,1,1,2), nrow = 5, ncol = 5, byrow = TRUE)
longData <- reshape2::melt(myData)
colnames(longData) <- c("Likelihood", "Consequence", "value")
longData <- mutate(longData, value = Consequence * Likelihood)
mycols <- rev(c("red4","red2","tomato2","orange","gold1","forestgreen"))
cols <- colorRampPalette(mycols)

myvals <- c(0, 8, 9, 10, 11, 25)
scaled_val <- scales::rescale(myvals, 0:1)

ggplot(longData, aes(x = Consequence, y = Likelihood, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colours = cols(length(mycols)), 
                       values = scaled_val) +
  theme(axis.text.y = element_text(angle = 90, hjust = 1, size = 20), legend.position = "none") +
  theme(axis.text.x = element_text(size = 20), legend.position = "none")+
  scale_x_continuous(name = "Impact", breaks = seq(1, 5, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Exposure", breaks = seq(1, 5, 1), expand = c(0, 0)) +
  geom_hline(yintercept = seq(1.5, 5.5)) +
  geom_vline(xintercept = seq(1.5, 5.5)) +
  coord_fixed()+
  theme(axis.title.x = element_text(color = "grey20", size = 20,face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, face = "plain"))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
pdf(NULL)
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure4",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off()

##########Terminal Migration Current (1-15)######
Area24_Terminal_C <- Area24_C %>% filter(`FWRA$LF_ID` >= 1 & `FWRA$LF_ID` <= 15)
VL <- c("Very Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area24_Terminal_C <- Area24_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  



cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_C)
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_C <- tidyr::pivot_longer(Area24_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                  values_to="Proportion")
colnames(Area24_Terminal_C)
Area24_Terminal_C$Rating <- factor(Area24_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

Area24_Terminal_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_Terminal_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_Terminal_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_Terminal_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_Terminal_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_Terminal_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_Terminal_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")

#Terminal Future
Area24_Terminal_F <- Area24_F %>% filter(`FWRA$LF_ID` >= 1 & `FWRA$LF_ID` <= 15)
VL <- c("Very Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area24_Terminal_F <- Area24_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_F)
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_F <- tidyr::pivot_longer(Area24_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_F)
Area24_Terminal_F$Rating <- factor(Area24_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

Area24_Terminal_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_Terminal_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_Terminal_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_Terminal_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_Terminal_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_Terminal_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_Terminal_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

###Combine C+F into one graph ---  here goes nothing
Area24_Terminal_C$Time <- rep("C", length(Area24_Terminal_C$LimitingFactor))

Area24_Terminal_F$Time <- rep("F", length(Area24_Terminal_F$LimitingFactor))

Area24_Terminal_F <- rbind(Area24_Terminal_C,Area24_Terminal_F)

ggplot(Area24_Terminal_F, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ LimitingFactor)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

library(tidyverse)

pdf(NULL)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures//", "Figure9",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 

#############Incubation (16-29)#####
Area24_Terminal_C <- Area24_C %>% filter(`FWRA$LF_ID` >= 16 & `FWRA$LF_ID` <= 29)
VL <- c("Very Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area24_Terminal_C <- Area24_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_C)
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_C <- tidyr::pivot_longer(Area24_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_C)
Area24_Terminal_C$Rating <- factor(Area24_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

Area24_Incubation_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_Incubation_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_Incubation_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_Incubation_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_Incubation_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_Incubation_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_Incubation_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")


#Incubation Future
Area24_Terminal_F <- Area24_F %>% filter(`FWRA$LF_ID` >= 16 & `FWRA$LF_ID` <= 29)
VL <- c("Very Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")

Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area24_Terminal_F <- Area24_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_F)
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_F <- tidyr::pivot_longer(Area24_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_F)
Area24_Terminal_F$Rating <- factor(Area24_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

Area24_Incubation_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_Incubation_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_Incubation_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_Incubation_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_Incubation_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_Incubation_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_Incubation_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

###Combine C+F into one graph ---  here goes nothing
Area24_Terminal_C$Time <- rep("C", length(Area24_Terminal_C$LimitingFactor))

Area24_Terminal_F$Time <- rep("F", length(Area24_Terminal_F$LimitingFactor))

Area24_Terminal_F <- rbind(Area24_Terminal_C,Area24_Terminal_F)

ggplot(Area24_Terminal_F, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ LimitingFactor)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

pdf(NULL)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure10",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off()

###############Freshwater Rearing (30-46)##############
###Freshwater Rearing Current
Area24_Terminal_C <- Area24_C %>% filter(`FWRA$LF_ID` >= 30 & `FWRA$LF_ID` <= 46)
VL <- c("Very Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area24_Terminal_C <- Area24_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_C)
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_C <- tidyr::pivot_longer(Area24_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_C)
Area24_Terminal_C$Rating <- factor(Area24_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

Area24_EarlyRearingRiver_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_EarlyRearingRiver_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_EarlyRearingRiver_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_EarlyRearingRiver_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_EarlyRearingRiver_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_EarlyRearingRiver_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_EarlyRearingRiver_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")


#Freshwater Rearing Future
Area24_Terminal_F <- Area24_F %>% filter(`FWRA$LF_ID` >= 30 & `FWRA$LF_ID` <= 46)
VL <- c("Very Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area24_Terminal_F <- Area24_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_F)
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_F <- tidyr::pivot_longer(Area24_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_F)
Area24_Terminal_F$Rating <- factor(Area24_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

Area24_EarlyRearingRiver_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_EarlyRearingRiver_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_EarlyRearingRiver_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_EarlyRearingRiver_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_EarlyRearingRiver_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_EarlyRearingRiver_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_EarlyRearingRiver_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

###Combine C+F into one graph ---  here goes nothing
Area24_Terminal_C$Time <- rep("C", length(Area24_Terminal_C$LimitingFactor))

Area24_Terminal_F$Time <- rep("F", length(Area24_Terminal_F$LimitingFactor))

Area24_Terminal_F <- rbind(Area24_Terminal_C,Area24_Terminal_F)

ggplot(Area24_Terminal_F, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ LimitingFactor)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

pdf(NULL)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure11",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 


################ Estuary Rearing (47-66)#########
# Estuary Rearing Current
Area24_Terminal_C <- Area24_C %>% filter(`FWRA$LF_ID` >= 47 & `FWRA$LF_ID` <= 66)
VL <- c("Very Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area24_Terminal_C <- Area24_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_C)
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"

require(reshape2)
Area24_Terminal_C <- tidyr::pivot_longer(Area24_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_C)
Area24_Terminal_C$Rating <- factor(Area24_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))


Area24_EarlyRearingEstuary_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_EarlyRearingEstuary_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_EarlyRearingEstuary_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_EarlyRearingEstuary_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_EarlyRearingEstuary_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_EarlyRearingEstuary_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_EarlyRearingEstuary_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")


#Early Rearing in the Estuary Future
Area24_Terminal_F <- Area24_F %>% filter(`FWRA$LF_ID` >= 47 & `FWRA$LF_ID` <= 66)
VL <- c("Very Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area24_Terminal_F <- Area24_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_F)
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_F <- tidyr::pivot_longer(Area24_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_F)
Area24_Terminal_F$Rating <- factor(Area24_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))


Area24_EarlyRearingEstuary_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_EarlyRearingEstuary_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_EarlyRearingEstuary_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_EarlyRearingEstuary_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_EarlyRearingEstuary_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_EarlyRearingEstuary_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_EarlyRearingEstuary_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")


###Combine C+F into one graph ---  here goes nothing
Area24_Terminal_C$Time <- rep("C", length(Area24_Terminal_C$LimitingFactor))

Area24_Terminal_F$Time <- rep("F", length(Area24_Terminal_F$LimitingFactor))

Area24_Terminal_F <- rbind(Area24_Terminal_C,Area24_Terminal_F)

ggplot(Area24_Terminal_F, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ LimitingFactor)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

pdf(NULL)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure12",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 



##############Biological Characteristics(67-70)#########
Area24_Terminal_C <- Area24_C %>% filter(`FWRA$LF_ID` >= 67 & `FWRA$LF_ID` <= 70)
VL <- c("Very Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area24_Terminal_C <- Area24_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_C)
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_C)[which(names(Area24_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_C <- tidyr::pivot_longer(Area24_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_C)
Area24_Terminal_C$Rating <- factor(Area24_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))


Area24_BiologicalCharacteristicsGenetics_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_BiologicalCharacteristicsGenetics_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_BiologicalCharacteristicsGenetics_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_BiologicalCharacteristicsGenetics_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_BiologicalCharacteristicsGenetics_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_BiologicalCharacteristicsGenetics_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_BiologicalCharacteristicsGenetics_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")


#Biological Characteristics - Future Rating
Area24_Terminal_F <- Area24_F %>% filter(`FWRA$LF_ID` >= 67 & `FWRA$LF_ID` <= 70)
VL <- c("Very Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area24_Terminal_F <- Area24_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area24_Terminal_F <- Area24_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area24_Terminal_F)
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area24_Terminal_F)[which(names(Area24_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area24_Terminal_F <- tidyr::pivot_longer(Area24_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area24_Terminal_F)
Area24_Terminal_F$Rating <- factor(Area24_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))


Area24_BiologicalCharacteristicsGenetics_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_BiologicalCharacteristicsGenetics_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_BiologicalCharacteristicsGenetics_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_BiologicalCharacteristicsGenetics_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_BiologicalCharacteristicsGenetics_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_BiologicalCharacteristicsGenetics_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_BiologicalCharacteristicsGenetics_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

###Combine C+F into one graph ---  here goes nothing
Area24_Terminal_C$Time <- rep("C", length(Area24_Terminal_C$LimitingFactor))

Area24_Terminal_F$Time <- rep("F", length(Area24_Terminal_F$LimitingFactor))

Area24_Terminal_F <- rbind(Area24_Terminal_C,Area24_Terminal_F)

ggplot(Area24_Terminal_F, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ LimitingFactor)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

pdf(NULL)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure13",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 


##########Ratings Current Stacked Bar########
VeryLow_All_C <- rbind(Area24_Terminal_CVL,Area24_Incubation_CVL,Area24_EarlyRearingRiver_CVL,Area24_EarlyRearingEstuary_CVL,Area24_BiologicalCharacteristicsGenetics_CVL)
Low_All_C <- rbind(Area24_Terminal_CL,Area24_Incubation_CL,Area24_EarlyRearingRiver_CL,Area24_EarlyRearingEstuary_CL,Area24_BiologicalCharacteristicsGenetics_CL)
Moderate_All_C <- rbind(Area24_Terminal_CM,Area24_Incubation_CM,Area24_EarlyRearingRiver_CM,Area24_EarlyRearingEstuary_CM,Area24_BiologicalCharacteristicsGenetics_CM)
High_All_C <- rbind(Area24_Terminal_CH,Area24_Incubation_CH,Area24_EarlyRearingRiver_CH,Area24_EarlyRearingEstuary_CH,Area24_BiologicalCharacteristicsGenetics_CH)
VeryHigh_All_C <- rbind(Area24_Terminal_CVH,Area24_Incubation_CVH,Area24_EarlyRearingRiver_CVH,Area24_EarlyRearingEstuary_CVH,Area24_BiologicalCharacteristicsGenetics_CVH)
Upper_C<-rbind(VeryLow_All_C,Low_All_C,Moderate_All_C,High_All_C,VeryHigh_All_C)
library(dplyr)

Upper_C<- Upper_C %>% mutate(Group =
                     case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                               between(LimitingFactor,16,29) ~ "Incubation",
                               between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                               between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                               between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)

Upper_C_Final <- Upper_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

Upper_C_Sums <- Upper_C_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_C_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)
Upper_C_Final$Proportion <- Upper_C_Final$newcount/Upper_C_Final$Denominator


##########Ratings Future Stacked Bar########
VeryLow_All_F <- rbind(Area24_Terminal_FVL,Area24_Incubation_FVL,Area24_EarlyRearingRiver_FVL,Area24_EarlyRearingEstuary_FVL,Area24_BiologicalCharacteristicsGenetics_FVL)
Low_All_F <- rbind(Area24_Terminal_FL,Area24_Incubation_FL,Area24_EarlyRearingRiver_FL,Area24_EarlyRearingEstuary_FL,Area24_BiologicalCharacteristicsGenetics_FL)
Moderate_All_F <- rbind(Area24_Terminal_FM,Area24_Incubation_FM,Area24_EarlyRearingRiver_FM,Area24_EarlyRearingEstuary_FM,Area24_BiologicalCharacteristicsGenetics_FM)
High_All_F <- rbind(Area24_Terminal_FH,Area24_Incubation_FH,Area24_EarlyRearingRiver_FH,Area24_EarlyRearingEstuary_FH,Area24_BiologicalCharacteristicsGenetics_FH)
VeryHigh_All_F <- rbind(Area24_Terminal_FVH,Area24_Incubation_FVH,Area24_EarlyRearingRiver_FVH,Area24_EarlyRearingEstuary_FVH,Area24_BiologicalCharacteristicsGenetics_FVH)


Upper_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F)
library(dplyr)

Upper_F<- Upper_F %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)

Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

Upper_F_Sums<- Upper_F_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))

Upper_F_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)

###Combine C+F into one graph ---  here goes nothing
Upper_C_Final$Time <- rep("C", length(Upper_C_Final$Rating))

Upper_F_Final$Time <- rep("F", length(Upper_F_Final$Rating))

Upper<- rbind(Upper_C_Final,Upper_F_Final)

unique(Upper$Group)

Upper$Group <- factor(Upper$Group, levels=c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics")) 

pdf(NULL)
library(ggplot2)

ggplot(Upper, aes(x=Time, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ Group)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Count of Ratings Grouped by Lifestage")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Area 24 - Count of Ratings Grouped by Lifestage",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 

##########All Current Stacked Bar########
VeryLow_All_C <- rbind(Area24_Terminal_CVL,Area24_Incubation_CVL,Area24_EarlyRearingRiver_CVL,Area24_EarlyRearingEstuary_CVL,Area24_BiologicalCharacteristicsGenetics_CVL)
Low_All_C <- rbind(Area24_Terminal_CL,Area24_Incubation_CL,Area24_EarlyRearingRiver_CL,Area24_EarlyRearingEstuary_CL,Area24_BiologicalCharacteristicsGenetics_CL)
Moderate_All_C <- rbind(Area24_Terminal_CM,Area24_Incubation_CM,Area24_EarlyRearingRiver_CM,Area24_EarlyRearingEstuary_CM,Area24_BiologicalCharacteristicsGenetics_CM)
High_All_C <- rbind(Area24_Terminal_CH,Area24_Incubation_CH,Area24_EarlyRearingRiver_CH,Area24_EarlyRearingEstuary_CH,Area24_BiologicalCharacteristicsGenetics_CH)
VeryHigh_All_C <- rbind(Area24_Terminal_CVH,Area24_Incubation_CVH,Area24_EarlyRearingRiver_CVH,Area24_EarlyRearingEstuary_CVH,Area24_BiologicalCharacteristicsGenetics_CVH)
Low_DG_C <- rbind(Area24_Terminal_CLPDG,Area24_Incubation_CLPDG,Area24_EarlyRearingRiver_CLPDG,Area24_EarlyRearingEstuary_CLPDG,Area24_BiologicalCharacteristicsGenetics_CLPDG)
High_DG_C <- rbind(Area24_Terminal_CHPDG,Area24_Incubation_CHPDG,Area24_EarlyRearingRiver_CHPDG,Area24_EarlyRearingEstuary_CHPDG,Area24_BiologicalCharacteristicsGenetics_CHPDG)
ALL_C<-rbind(VeryLow_All_C,Low_All_C,Moderate_All_C,High_All_C,VeryHigh_All_C,Low_DG_C,High_DG_C)

library(dplyr)

ALL_C<- ALL_C %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)


ALL_C <- ALL_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_C_Sums<- ALL_C %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_C$Denominator <- c(200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150)

ALL_C$Proportion <- ALL_C$newcount/ALL_C$Denominator


##########All Future Stacked Bar########
VeryLow_All_F <- rbind(Area24_Terminal_FVL,Area24_Incubation_FVL,Area24_EarlyRearingRiver_FVL,Area24_EarlyRearingEstuary_FVL,Area24_BiologicalCharacteristicsGenetics_FVL)
Low_All_F <- rbind(Area24_Terminal_FL,Area24_Incubation_FL,Area24_EarlyRearingRiver_FL,Area24_EarlyRearingEstuary_FL,Area24_BiologicalCharacteristicsGenetics_FL)
Moderate_All_F <- rbind(Area24_Terminal_FM,Area24_Incubation_FM,Area24_EarlyRearingRiver_FM,Area24_EarlyRearingEstuary_FM,Area24_BiologicalCharacteristicsGenetics_FM)
High_All_F <- rbind(Area24_Terminal_FH,Area24_Incubation_FH,Area24_EarlyRearingRiver_FH,Area24_EarlyRearingEstuary_FH,Area24_BiologicalCharacteristicsGenetics_FH)
VeryHigh_All_F <- rbind(Area24_Terminal_FVH,Area24_Incubation_FVH,Area24_EarlyRearingRiver_FVH,Area24_EarlyRearingEstuary_FVH,Area24_BiologicalCharacteristicsGenetics_FVH)
Low_DG_F <- rbind(Area24_Terminal_FLPDG,Area24_Incubation_FLPDG,Area24_EarlyRearingRiver_FLPDG,Area24_EarlyRearingEstuary_FLPDG,Area24_BiologicalCharacteristicsGenetics_FLPDG)
High_DG_F <- rbind(Area24_Terminal_FHPDG,Area24_Incubation_FHPDG,Area24_EarlyRearingRiver_FHPDG,Area24_EarlyRearingEstuary_FHPDG,Area24_BiologicalCharacteristicsGenetics_FHPDG)

ALL_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F,Low_DG_F,High_DG_F)
library(dplyr)

ALL_F<- ALL_F %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)

ALL_F <- ALL_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_F_Sums<- ALL_F %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_F$Denominator <- c(200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150)

ALL_F$Proportion <- ALL_F$newcount/ALL_F$Denominator

###Combine C+F into one graph ---  here goes nothing
ALL_C$Time <- rep("C", length(ALL_C$Rating))

ALL_F$Time <- rep("F", length(ALL_F$Rating))

ALL<- rbind(ALL_C,ALL_F)

unique(ALL$Group)

ALL$Group <- factor(ALL$Group, levels=c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics")) 

BiologicalCharacteristics <- filter(ALL, Group == "Biological Characteristics")

ALL <- subset(ALL, Group != "Biological Characteristics")


#Upper$Group <- revalue(Upper$Group , c("Terminal" = "Terminal Migration (n = )",
                                       "Incubation" = "Incubation (n = 50)", 
                                       "Freshwater Rearing" = "Freshwater Rearing (n = 53)",
                                       "Estuary Rearing" = "Estuary Rearing (n = 62)", 
                                       "Biological Characteristics" = "Biological Characteristics (n = 12)"))
dev.off()
pdf(NULL)
library(ggplot2)

ggplot(ALL, aes(x=Time, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ Group)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,200), breaks=seq(0,200,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure 6",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 

###Biological Characteristics Category
ggplot(BiologicalCharacteristics, aes(x=Time, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ Group)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,40), breaks=seq(0,40,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure 7",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 




##########Upper Current Stacked Bar########
Moderate_All_C <- rbind(Area24_Terminal_CM,Area24_Incubation_CM,Area24_EarlyRearingRiver_CM,Area24_EarlyRearingEstuary_CM,Area24_BiologicalCharacteristicsGenetics_CM)
High_All_C <- rbind(Area24_Terminal_CH,Area24_Incubation_CH,Area24_EarlyRearingRiver_CH,Area24_EarlyRearingEstuary_CH,Area24_BiologicalCharacteristicsGenetics_CH)
VeryHigh_All_C <- rbind(Area24_Terminal_CVH,Area24_Incubation_CVH,Area24_EarlyRearingRiver_CVH,Area24_EarlyRearingEstuary_CVH,Area24_BiologicalCharacteristicsGenetics_CVH)


Upper_C<-rbind(Moderate_All_C,High_All_C,VeryHigh_All_C)
library(dplyr)

Upper_C<- Upper_C %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)

Upper_C_Final <- Upper_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 


Upper_C_Sums<- Upper_C_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_C_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)
Upper_C_Final$Proportion <- Upper_C_Final$newcount/Upper_C_Final$Denominator


##########Upper Future Stacked Bar########
Moderate_All_F <- rbind(Area24_Terminal_FM,Area24_Incubation_FM,Area24_EarlyRearingRiver_FM,Area24_EarlyRearingEstuary_FM,Area24_BiologicalCharacteristicsGenetics_FM)
High_All_F <- rbind(Area24_Terminal_FH,Area24_Incubation_FH,Area24_EarlyRearingRiver_FH,Area24_EarlyRearingEstuary_FH,Area24_BiologicalCharacteristicsGenetics_FH)
VeryHigh_All_F <- rbind(Area24_Terminal_FVH,Area24_Incubation_FVH,Area24_EarlyRearingRiver_FVH,Area24_EarlyRearingEstuary_FVH,Area24_BiologicalCharacteristicsGenetics_FVH)
Upper_F<-rbind(Moderate_All_F,High_All_F,VeryHigh_All_F)
library(dplyr)

Upper_F<- Upper_F %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)
Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

###Combine C+F into one graph ---  here goes nothing
Upper_C$Time <- rep("C", length(Upper_C$Rating))

Upper_F$Time <- rep("F", length(Upper_F$Rating))

ALL<- rbind(Upper_C,Upper_F)

unique(ALL$Group)

ALL$Group <- factor(ALL$Group, levels=c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics")) 

#Upper$Group <- revalue(Upper$Group , c("Terminal" = "Terminal Migration (n = )",
#"Incubation" = "Incubation (n = 50)", 
#"Freshwater Rearing" = "Freshwater Rearing (n = 53)",
#"Estuary Rearing" = "Estuary Rearing (n = 62)", 
#"Biological Characteristics" = "Biological Characteristics (n = 12)"))
pdf(NULL)
library(ggplot2)

ALL <- subset(ALL, Group != "Biological Characteristics")
view(ALL)
dev.off()

ggplot(ALL, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ Group)+
  labs(x = "Rating Period", y = "Count") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,30), breaks=seq(0,30,5))


#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure 5",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 

##########DG Current & Future Stacked Bar########
Low_DG_C <- rbind(Area24_Terminal_CLPDG,Area24_Incubation_CLPDG,Area24_EarlyRearingRiver_CLPDG,Area24_EarlyRearingEstuary_CLPDG,Area24_BiologicalCharacteristicsGenetics_CLPDG)
High_DG_C <- rbind(Area24_Terminal_CHPDG,Area24_Incubation_CHPDG,Area24_EarlyRearingRiver_CHPDG,Area24_EarlyRearingEstuary_CHPDG,Area24_BiologicalCharacteristicsGenetics_CHPDG)
DG_C<-rbind(Low_DG_C,High_DG_C)

Low_DG_F <- rbind(Area24_Terminal_FLPDG,Area24_Incubation_FLPDG,Area24_EarlyRearingRiver_FLPDG,Area24_EarlyRearingEstuary_FLPDG,Area24_BiologicalCharacteristicsGenetics_FLPDG)
High_DG_F <- rbind(Area24_Terminal_FHPDG,Area24_Incubation_FHPDG,Area24_EarlyRearingRiver_FHPDG,Area24_EarlyRearingEstuary_FHPDG,Area24_BiologicalCharacteristicsGenetics_FHPDG)
DG_F<-rbind(Low_DG_F,High_DG_F)

DG_CF<-rbind(DG_C,DG_F)

library(dplyr)

library(dplyr)

DG_CF<- DG_CF %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)
DG_CF <- DG_CF %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

DG_CF_Sums<- DG_CF %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


DG_CF$Denominator <- c(294,216,56,140,120,294,216,56,140,120)
DG_CF$Proportion <- DG_CF$newcount/DG_CF$Denominator



DG_CF <- subset(DG_CF, Group != "Biological Characteristics")


ggplot(DG_CF, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Count of Data Gaps") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing"), labels = c("Terminal\n Migration\n (n = 120)","Incubation\n (n = 140)","Freshwater\nRearing\n (n = 216)","Estuary\nRearing\n (n = 294)","Biological \n Characteristics & \n Genetics\n (n = 56)"))+
  scale_fill_manual(values = c("Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,300), breaks=seq(0,500,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure8",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


######Proportional Data Gap Life Stage Breakdown - Current & Future All############
Low_DG_C <- rbind(Area24_Terminal_CLPDG,Area24_Incubation_CLPDG,Area24_EarlyRearingRiver_CLPDG,Area24_EarlyRearingEstuary_CLPDG,Area24_BiologicalCharacteristicsGenetics_CLPDG)
High_DG_C <- rbind(Area24_Terminal_CHPDG,Area24_Incubation_CHPDG,Area24_EarlyRearingRiver_CHPDG,Area24_EarlyRearingEstuary_CHPDG,Area24_BiologicalCharacteristicsGenetics_CHPDG)
Low_DG_F <- rbind(Area24_Terminal_FLPDG,Area24_Incubation_FLPDG,Area24_EarlyRearingRiver_FLPDG,Area24_EarlyRearingEstuary_FLPDG,Area24_BiologicalCharacteristicsGenetics_FLPDG)
High_DG_F <- rbind(Area24_Terminal_FHPDG,Area24_Incubation_FHPDG,Area24_EarlyRearingRiver_FHPDG,Area24_EarlyRearingEstuary_FHPDG,Area24_BiologicalCharacteristicsGenetics_FHPDG)
DG_F<-rbind(Low_DG_F,High_DG_F)
DG_C<-rbind(Low_DG_C,High_DG_C)

DG_CF<-rbind(DG_C,DG_F)

DG_CF<- DG_CF %>% mutate(Group =
                         case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                   between(LimitingFactor,16,29) ~ "Incubation",
                                   between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                   between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                   between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)
DG_CF <- DG_CF %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 
View(DG_CF)
DG_CF_Sums<- DG_CF %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


DG_CF$Denominator <- c(56,294,216,140,120,56,294,216,140,120)
DG_CF$Proportion <- DG_CF$newcount/DG_CF$Denominator


ggplot(DG_CF, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Data Gaps Grouped by Lifestage - Current & Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics"), labels = c("Terminal\n Migration\n (n = 120)","Incubation\n (n = 140)","Freshwater\nRearing\n (n = 216)","Estuary\nRearing\n (n = 294)","Biological \n Characteristics & \n Genetics\n (n = 56)"))+
  scale_fill_manual(values = c("Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure7",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

##########All Current Proprtion Bar########
VeryLow_All_C <- rbind(Area24_Terminal_CVL,Area24_Incubation_CVL,Area24_EarlyRearingRiver_CVL,Area24_EarlyRearingEstuary_CVL,Area24_BiologicalCharacteristicsGenetics_CVL)
Low_All_C <- rbind(Area24_Terminal_CL,Area24_Incubation_CL,Area24_EarlyRearingRiver_CL,Area24_EarlyRearingEstuary_CL,Area24_BiologicalCharacteristicsGenetics_CL)
Moderate_All_C <- rbind(Area24_Terminal_CM,Area24_Incubation_CM,Area24_EarlyRearingRiver_CM,Area24_EarlyRearingEstuary_CM,Area24_BiologicalCharacteristicsGenetics_CM)
High_All_C <- rbind(Area24_Terminal_CH,Area24_Incubation_CH,Area24_EarlyRearingRiver_CH,Area24_EarlyRearingEstuary_CH,Area24_BiologicalCharacteristicsGenetics_CH)
VeryHigh_All_C <- rbind(Area24_Terminal_CVH,Area24_Incubation_CVH,Area24_EarlyRearingRiver_CVH,Area24_EarlyRearingEstuary_CVH,Area24_BiologicalCharacteristicsGenetics_CVH)
Low_DG_C <- rbind(Area24_Terminal_CLPDG,Area24_Incubation_CLPDG,Area24_EarlyRearingRiver_CLPDG,Area24_EarlyRearingEstuary_CLPDG,Area24_BiologicalCharacteristicsGenetics_CLPDG)
High_DG_C <- rbind(Area24_Terminal_CHPDG,Area24_Incubation_CHPDG,Area24_EarlyRearingRiver_CHPDG,Area24_EarlyRearingEstuary_CHPDG,Area24_BiologicalCharacteristicsGenetics_CHPDG)

ALL_C<-rbind(VeryLow_All_C,Low_All_C,Moderate_All_C,High_All_C,VeryHigh_All_C,Low_DG_C,High_DG_C)
library(dplyr)

ALL_C<- ALL_C %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)

ALL_C <- ALL_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_C_Sums<- ALL_C %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_C$Denominator <- c(40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150)

ALL_C$Proportion <- ALL_C$newcount/ALL_C$Denominator

##########All Future Stacked Bar########
VeryLow_All_F <- rbind(Area24_Terminal_FVL,Area24_Incubation_FVL,Area24_EarlyRearingRiver_FVL,Area24_EarlyRearingEstuary_FVL,Area24_BiologicalCharacteristicsGenetics_FVL)
Low_All_F <- rbind(Area24_Terminal_FL,Area24_Incubation_FL,Area24_EarlyRearingRiver_FL,Area24_EarlyRearingEstuary_FL,Area24_BiologicalCharacteristicsGenetics_FL)
Moderate_All_F <- rbind(Area24_Terminal_FM,Area24_Incubation_FM,Area24_EarlyRearingRiver_FM,Area24_EarlyRearingEstuary_FM,Area24_BiologicalCharacteristicsGenetics_FM)
High_All_F <- rbind(Area24_Terminal_FH,Area24_Incubation_FH,Area24_EarlyRearingRiver_FH,Area24_EarlyRearingEstuary_FH,Area24_BiologicalCharacteristicsGenetics_FH)
VeryHigh_All_F <- rbind(Area24_Terminal_FVH,Area24_Incubation_FVH,Area24_EarlyRearingRiver_FVH,Area24_EarlyRearingEstuary_FVH,Area24_BiologicalCharacteristicsGenetics_FVH)
Low_DG_F <- rbind(Area24_Terminal_FLPDG,Area24_Incubation_FLPDG,Area24_EarlyRearingRiver_FLPDG,Area24_EarlyRearingEstuary_FLPDG,Area24_BiologicalCharacteristicsGenetics_FLPDG)
High_DG_F <- rbind(Area24_Terminal_FHPDG,Area24_Incubation_FHPDG,Area24_EarlyRearingRiver_FHPDG,Area24_EarlyRearingEstuary_FHPDG,Area24_BiologicalCharacteristicsGenetics_FHPDG)

ALL_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F,Low_DG_F,High_DG_F)
library(dplyr)

ALL_F<- ALL_F %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Biological Characteristics"))
library(dplyr)

ALL_F <- ALL_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_F_Sums<- ALL_F %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_F$Denominator <- c(40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150,40,200,170,120,150)
ALL_F$Proportion <- ALL_F$newcount/ALL_F$Denominator


###Combine C+F into one graph ---  here goes nothing
ALL_C$Time <- rep("C", length(ALL_C$Rating))

ALL_F$Time <- rep("F", length(ALL_F$Rating))

ALL<- rbind(ALL_C,ALL_F)

unique(ALL$Group)

ALL$Group <- factor(ALL$Group, levels=c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics")) 
view(ALL)
#Upper$Group <- revalue(Upper$Group , c("Terminal" = "Terminal Migration (n = )",
#"Incubation" = "Incubation (n = 50)", 
#"Freshwater Rearing" = "Freshwater Rearing (n = 53)",
#"Estuary Rearing" = "Estuary Rearing (n = 62)", 
#"Biological Characteristics" = "Biological Characteristics (n = 12)"))
pdf(NULL)
library(ggplot2)

ggplot(ALL, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+ facet_grid(~ Group)+
  labs(x = "Rating Period", y = "Percentage") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Proportion of Data Gaps and Risk Ratings Grouped by Lifestage")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure 4",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
dev.off() 