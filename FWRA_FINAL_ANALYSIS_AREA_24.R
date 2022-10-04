######Intro#######
#Hi team, welcome to our R script for creating plots for Area 25. To follow along
#view my comments in the "#" regions, these will provide you a guide for what to
#change in your own analysis

#GUIDE#
#Areas where you will need to make changes are indicated with "#$#", copy 
#and paste this text and paste it into the find function (command F), to view
#all the areas where you need to make changes

#whenever begging an R script, make sure to write down key infromation to the script
#such as its title, author, date updated, etc. Example below for this script:

#Title: Area 25 Plots Template
#Author: Critty (Christian) Carson
#Last updated : November 21th, 2021
#Description : Summaries of NUSEDS escapement, hydromet data, 

#all scripts should start with installing packages, which are basically mini programs
#that you can add within R to help you do analyses or make specific kinds of plots

#the packages you need are below, unhashtag the sentance below to install packages 
#hit cmd + enter to run this code or highlight it and click run in the top right corner

#install.packages(c("boot", "MASS","plyr","dplyr", "plot2", "tibble", "car", "reshape2",
#                  "epitools", "readxl", "tidyverse","arsenal")))

#now we need to load all the packages into our r script, this is done with the
#function, library(). For instance, if we want to load the package "boot", we would
#run the function below. Now, highlight all the functions below in a row and run them
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(plot2)
library(tibble)
library(car)
library(reshape2)
library(epitools)
library(readxl)
library(tidyverse)
library(readr)
library(arsenal)
source("https://raw.githubusercontent.com/koundy/ggplot_theme_Publication/master/ggplot_theme_Publication-2.R")
#install.packages(c("ggplot2", "patchwork", "palmerpenguins"))
library(tidyverse)
library(patchwork)
library(palmerpenguins)
library(viridis)
# <- "https://raw.githubusercontent.com/gadenbuie/yule-rstudio/master/Yule-RStudio.rstheme"
#rstudioapi::addTheme(yule_theme, apply = TRUE)


#ok, so we have loaded all out packages and we are now ready to import data for
#specific analyses. First, we must designate a section; this is done bye putting 5 or more "#"'s in a row
#below we are going to analyize escapement data from the NUseds database, so we naming the section as:

#####Setup#####

#--------------any libraries needed are loaded and displayed below--------------
#
library(dplyr)
library("zoo")
#
#--------------make project folders and folder paths----------------------------


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
#go to file, import data, import data from excel, and import your data
#copy and paste the output from the console tab below, like I did here
#alternativley, just run my code below
#make sure you name and assign your new spreadsheet, below I assign this import
#by naming it "nuseds" below, now when I use the name nuseds, it will be called
#on in the program
data.path <- paste(wd, "/", "Data", sep = "")



# time to upload the datas
FWRA <- read.csv(paste(data.path,"/", "R_Ready_Final_FWRA_Results.csv",
                         sep = ""), stringsAsFactors = FALSE)
colnames(FWRA)
FWRA <- subset(FWRA, LF_ID != 23 & LF_ID != 24)



#to view the spread sheet, type view(nuseds)
#to get a list og all the different varaibles or coloumn names run the code below
colnames(FWRA)

Current <- FWRA %>% dplyr:: select(ends_with("_C"))
Future <- FWRA %>% dplyr:: select(ends_with("_F"))

require(dplyr)
colnames(FWRA)
Area23 <- FWRA %>% dplyr:: select(Sarita_C:Somass_F)
Area23 <- cbind(FWRA$LF_ID,Area23)
Area23_C <- Area23 %>% dplyr:: select(ends_with("_C"))
Area23_C <- cbind(FWRA$LF_ID,Area23_C)
Area23_F <- Area23 %>% dplyr:: select(ends_with("_F"))
Area23_F <- cbind(FWRA$LF_ID,Area23_F)

Area24 <- FWRA %>% dplyr:: select(Megin_C:Muriel.Creek_F)
Area24 <- cbind(FWRA$LF_ID,Area24)
Area24_C <- Area24 %>% dplyr:: select(ends_with("_C"))
Area24_C <- cbind(FWRA$LF_ID,Area24_C)
Area24_F <- Area24 %>% dplyr:: select(ends_with("_F"))
Area24_F <- cbind(FWRA$LF_ID,Area24_F)
colnames(Area24)

Area25 <- FWRA %>% dplyr:: select(Tahsis.River_C:Conuma_F)
Area25 <- cbind(FWRA$LF_ID,Area25)
Area25_C <- Area25 %>% dplyr:: select(ends_with("_C"))
Area25_C <- cbind(FWRA$LF_ID,Area25_C)
Area25_F <- Area25 %>% dplyr:: select(ends_with("_F"))
Area25_F <- cbind(FWRA$LF_ID,Area25_F)

Area26 <- FWRA %>% select(Kaouk_C:Artlish_F)
Area26 <- cbind(FWRA$LF_ID,Area26)
Area26_C <- Area26 %>% dplyr:: select(ends_with("_C"))
Area26_C <- cbind(FWRA$LF_ID,Area26_C)
Area26_F <- Area26 %>% dplyr:: select(ends_with("_F"))
Area26_F <- cbind(FWRA$LF_ID,Area26_F)



#Converting to Counts in the same order

Area23_C_Sums <- Area23_C %>% select(ends_with("_C")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)
Area23_C_Sums$value <- factor(Area23_C_Sums$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))

Area23_F_Sums <- Area23_F %>% select(ends_with("_F")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)
Area23_F_Sums$value <- factor(Area23_F_Sums$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))

#Area 24
Area24_C_Sums <- Area24_C %>% select(ends_with("_C")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)
Area24_C_Sums$value <- factor(Area24_C_Sums$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))

Area24_F_Sums <- Area24_F %>% select(ends_with("_F")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)
Area24_F_Sums$value <- factor(Area24_F_Sums$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))

#Area 25
Area25_C_Sums <- Area25_C %>% select(ends_with("_C")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)

Area25_F_Sums <- Area25_F %>% select(ends_with("_F")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)

#Area 26
Area26_C_Sums <- Area26_C %>% select(ends_with("_C")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)

Area26_F_Sums <- Area26_F %>% select(ends_with("_F")) %>%
  gather(key, value, na.rm = TRUE) %>%
  count(value)

#Compiling Area 24
Area24_C_Sums <- Area24_C_Sums %>% 
  add_column(Time = "Current",
             .after = "n")
Area24_F_Sums <- Area24_F_Sums %>% 
  add_column(Time = "Future",
             .after = "n")

Area24_combo <- rbind(Area24_C_Sums,Area24_F_Sums)
cols <- c("Very\n Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very\n High" = "red4", "Low Priority\n Data Gap" = "grey70", "High Priority\n Data Gap" = "purple")

Area24_combo %>%
  ggplot(aes(x=value, y = n, fill = Time)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(x = "Rating", y = "Count") + 
  theme_Publication()+
  scale_fill_viridis_d(option="magma",direction = -1,begin = 0.2, end = .80)+ ggtitle("Area 24 - Count of Risk Rankings Across All LF's") 
print(Area24_combo)

Area24_combo$denominator <- c(rep(680,14))
head(Area24_combo)

Area24_combo$Proportion <- Area24_combo$n/Area24_combo$denominator
Area24_combo$Combined <- c(rep("Combined",14))

colnames(Area24_combo)[which(names(Area24_combo) == "value")] <- "Rating"



ggplot(Area24_combo, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Rating Period", y = "Percentage") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Proportion of Risk Ratings and Data Gaps")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Current", "Future"), labels = c("Current \n (n = 680)","Future  \n (n = 680)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"),limits = c("Very High", "High", "Moderate", "High Priority Data Gap","Low","Very Low", "Low Priority Data Gap"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure2",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

4#Combined Current + Future
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
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure1",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
##########Terminal Migration Current (1-15)######
Area24_Terminal_C <- Area24_C %>% filter(row(Area24_C) >= 1 & row(Area24_C) <= 15)
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



ggplot(Area24_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Terminal Migration - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 16),breaks=seq(1,15,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure8_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

Area24_Terminal_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_Terminal_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_Terminal_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_Terminal_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_Terminal_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_Terminal_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_Terminal_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")




#Terminal Future
Area24_Terminal_F <- Area24_F %>% filter(row(Area24_F) >= 1 & row(Area24_F) <= 15)
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

ggplot(Area24_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Terminal Migration - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 16),breaks=seq(1,15,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure8_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

Area24_Terminal_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_Terminal_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_Terminal_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_Terminal_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_Terminal_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_Terminal_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_Terminal_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

colnames(Area24_Terminal_C)

Area24_Terminal_C <- Area24_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area24_Terminal_F <- Area24_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area24_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area24_Terminal_F %>%
  mutate(prop = prop.table(Frequency))


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


ggplot(Area24_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Incubation - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(15, 30),breaks=seq(16,29,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure9_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

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

ggplot(Area24_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Incubation - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(15, 30),breaks=seq(16,29,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure9_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

Area24_Incubation_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_Incubation_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_Incubation_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_Incubation_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_Incubation_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_Incubation_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_Incubation_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")


Area24_Terminal_C <- Area24_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area24_Terminal_F <- Area24_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area24_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area24_Terminal_F %>%
  mutate(prop = prop.table(Frequency))

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



ggplot(Area24_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Freshwater Rearing - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(29, 47),breaks=seq(30,46,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure10_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

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

ggplot(Area24_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Freshwater Rearing - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(29, 47),breaks=seq(30,46,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure10_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)



Area24_EarlyRearingRiver_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_EarlyRearingRiver_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_EarlyRearingRiver_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_EarlyRearingRiver_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_EarlyRearingRiver_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_EarlyRearingRiver_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_EarlyRearingRiver_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")
Area24_Terminal_C <- Area24_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area24_Terminal_F <- Area24_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area24_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area24_Terminal_F %>%
  mutate(prop = prop.table(Frequency))

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



ggplot(Area24_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Estuary Rearing - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(46, 67),breaks=seq(47,66,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure11_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


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

ggplot(Area24_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Estuary Rearing - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(46, 67),breaks=seq(47,66,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure11_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

Area24_EarlyRearingEstuary_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_EarlyRearingEstuary_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_EarlyRearingEstuary_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_EarlyRearingEstuary_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_EarlyRearingEstuary_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_EarlyRearingEstuary_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_EarlyRearingEstuary_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

Area24_Terminal_C <- Area24_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area24_Terminal_F <- Area24_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area24_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area24_Terminal_F %>%
  mutate(prop = prop.table(Frequency))


##############Biological Characteristics & Genetics(67-70)#########
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



ggplot(Area24_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Biological Characteristics & Genetics - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(66, 71),breaks=seq(67,70,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure12_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area24_BiologicalCharacteristicsGenetics_CVL <- subset(Area24_Terminal_C, Rating == "Very Low")
Area24_BiologicalCharacteristicsGenetics_CL <- subset(Area24_Terminal_C, Rating == "Low")
Area24_BiologicalCharacteristicsGenetics_CM <- subset(Area24_Terminal_C, Rating == "Moderate")
Area24_BiologicalCharacteristicsGenetics_CH <- subset(Area24_Terminal_C, Rating == "High")
Area24_BiologicalCharacteristicsGenetics_CVH <- subset(Area24_Terminal_C, Rating == "Very High")
Area24_BiologicalCharacteristicsGenetics_CHPDG <- subset(Area24_Terminal_C, Rating == "High Priority Data Gap")
Area24_BiologicalCharacteristicsGenetics_CLPDG <- subset(Area24_Terminal_C, Rating == "Low Priority Data Gap")


#Biological Characteristics & Genetics - Future Rating
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

ggplot(Area24_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Biological Characteristics & Genetics - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(66, 71),breaks=seq(67,70,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,15,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure12_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area24_BiologicalCharacteristicsGenetics_FVL <- subset(Area24_Terminal_F, Rating == "Very Low")
Area24_BiologicalCharacteristicsGenetics_FL <- subset(Area24_Terminal_F, Rating == "Low")
Area24_BiologicalCharacteristicsGenetics_FM <- subset(Area24_Terminal_F, Rating == "Moderate")
Area24_BiologicalCharacteristicsGenetics_FH <- subset(Area24_Terminal_F, Rating == "High")
Area24_BiologicalCharacteristicsGenetics_FVH <- subset(Area24_Terminal_F, Rating == "Very High")
Area24_BiologicalCharacteristicsGenetics_FHPDG <- subset(Area24_Terminal_F, Rating == "High Priority Data Gap")
Area24_BiologicalCharacteristicsGenetics_FLPDG <- subset(Area24_Terminal_F, Rating == "Low Priority Data Gap")

Area24_Terminal_C <- Area24_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area24_Terminal_F <- Area24_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area24_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area24_Terminal_F %>%
  mutate(prop = prop.table(Frequency))


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
                               between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

Upper_C_Final <- Upper_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

Upper_C_Sums <- Upper_C_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_C_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)
Upper_C_Final$Proportion <- Upper_C_Final$newcount/Upper_C_Final$Denominator

ggplot(Upper_C_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Ratings Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 90)","Incubation\n (n = 50)","Freshwater\nRearing \n (n = 62)","Estuary\nRearing\n (n = 53)","Biological \n Characteristics & \n Genetics\n (n = 12)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10))
"53,62,12,50,90"

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
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

Upper_F_Sums<- Upper_F_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))

Upper_F_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)

Upper_F_Final$Proportion <- Upper_F_Final$newcount/Upper_F_Final$Denominator
ggplot(Upper_F_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Ratings Grouped by Lifestage - Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 90)","Incubation\n (n = 50)","Freshwater\nRearing \n (n = 62)","Estuary\nRearing\n (n = 53)","Biological \n Characteristics & \n Genetics\n (n = 12)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10))
"53,62,12,50,90"


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
                                     between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)


ALL_C <- ALL_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_C_Sums<- ALL_C %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_C$Denominator <- c(200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150)

ALL_C$Proportion <- ALL_C$newcount/ALL_C$Denominator
ggplot(ALL_C, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Ratings & Data Gaps Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 150)","Incubation\n (n = 120)","Freshwater\nRearing \n (n = 170)","Estuary\nRearing\n (n = 200)","Biological \n Characteristics & \n Genetics\n (n = 40)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,200), breaks=seq(0,200,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure3_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

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
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

ALL_F <- ALL_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_F_Sums<- ALL_F %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_F$Denominator <- c(200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150)

ALL_F$Proportion <- ALL_F$newcount/ALL_F$Denominator
ggplot(ALL_F, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Ratings & Data Gaps Grouped by Lifestage - Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 150)","Incubation\n (n = 120)","Freshwater\nRearing \n (n = 170)","Estuary\nRearing\n (n = 200)","Biological \n Characteristics & \n Genetics\n (n = 40)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,200), breaks=seq(0,200,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure3_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
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
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

Upper_C_Final <- Upper_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 


Upper_C_Sums<- Upper_C_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_C_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)
Upper_C_Final$Proportion <- Upper_C_Final$newcount/Upper_C_Final$Denominator

ggplot(Upper_C_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - High Risk LF's Grouped by Lifestage - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 90)","Incubation\n (n = 50)","Freshwater\nRearing \n (n = 62)","Estuary\nRearing\n (n = 53)","Biological \n Characteristics & \n Genetics\n (n = 12)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,30), breaks=seq(0,30,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure5_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

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
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)
Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ggplot(Upper_F_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - High Risk LF's Grouped by Lifestage - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 90)","Incubation\n (n = 50)","Freshwater\nRearing \n (n = 62)","Estuary\nRearing\n (n = 53)","Biological \n Characteristics & \n Genetics\n (n = 12)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,30), breaks=seq(0,30,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure5_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

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
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)
DG_CF <- DG_CF %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

DG_CF_Sums<- DG_CF %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


DG_CF$Denominator <- c(294,216,56,140,120,294,216,56,140,120)
DG_CF$Proportion <- DG_CF$newcount/DG_CF$Denominator


ggplot(DG_CF, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Count of Data Gaps") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Data Gaps Grouped by Lifestage - Current & Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 120)","Incubation\n (n = 140)","Freshwater\nRearing\n (n = 216)","Estuary\nRearing\n (n = 294)","Biological \n Characteristics & \n Genetics\n (n = 56)"))+
  scale_fill_manual(values = c("Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,300), breaks=seq(0,500,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure6",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


###########Proportional Life Stage Breakdown - Future All############
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
                                         between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 


Upper_F_Sums<- Upper_F_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_F_Final$Denominator <- c(53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90,53,62,12,50,90)
Upper_F_Final$Proportion <- Upper_F_Final$newcount/Upper_F_Final$Denominator

ggplot(Upper_F_Final, aes(x=Group, y=Proportion, fill = Rating, label = newcount))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - LF's Grouped by Lifestage - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 90)","Incubation\n (n = 50)","Freshwater\nRearing \n (n = 62)","Estuary\nRearing\n (n = 53)","Biological \n Characteristics & \n Genetics\n (n = 12)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)


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
                                   between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)
DG_CF <- DG_CF %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

DG_CF_Sums<- DG_CF %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


DG_CF$Denominator <- c(294,216,56,140,120,294,216,56,140,120)
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
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 120)","Incubation\n (n = 140)","Freshwater\nRearing\n (n = 216)","Estuary\nRearing\n (n = 294)","Biological \n Characteristics & \n Genetics\n (n = 56)"))+
  scale_fill_manual(values = c("Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)
#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure7",".jpeg"),
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
                                     between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

ALL_C <- ALL_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_C_Sums<- ALL_C %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_C$Denominator <- c(200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150)

ALL_C$Proportion <- ALL_C$newcount/ALL_C$Denominator
ggplot(ALL_C, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Ratings & Data Gaps Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 150)","Incubation\n (n = 120)","Freshwater\nRearing \n (n = 170)","Estuary\nRearing\n (n = 200)","Biological \n Characteristics & \n Genetics\n (n = 40)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)


#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure4_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
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
                                     between(LimitingFactor,66,70) ~ "Biological Characteristics & Genetics"))
library(dplyr)

ALL_F <- ALL_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_F_Sums<- ALL_F %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_F$Denominator <- c(200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150,200,170,40,120,150)

ALL_F$Proportion <- ALL_F$newcount/ALL_F$Denominator
ggplot(ALL_F, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 24 - Ratings & Data Gaps Grouped by Lifestage - Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Biological Characteristics & Genetics"), labels = c("Terminal\n Migration\n (n = 150)","Incubation\n (n = 120)","Freshwater\nRearing \n (n = 170)","Estuary\nRearing\n (n = 200)","Biological \n Characteristics & \n Genetics\n (n = 40)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_24_Figures/", "Figure4_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


