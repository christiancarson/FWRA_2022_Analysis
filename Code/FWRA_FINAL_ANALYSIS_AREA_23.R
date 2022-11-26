######Intro#######
#Hi team, welcome to our R script for creating plots for Area 23. To follow along
#view my comments in the "#" regions, these will provide you a guide for what to
#change in your own analysis

#GUIDE#
#Areas where you will need to make changes are indicated with "#$#", copy 
#and paste this text and paste it into the find function (command F), to view
#all the areas where you need to make changes

#whenever begging an R script, make sure to write down key infromation to the script
#such as its title, author, date updated, etc. Example below for this script:

#Title: Area 23 Plots Template
#Author: Critty (Christian) Carson
#Last updated : August 19th, 2022
#Description : Summaries of NUSEDS escapement, hydromet data, 

#all scripts should start with installing packages, which are basically mini programs
#that you can add within R to help you do analyses or make specific kinds of plots

#the packages you need are below, unhashtag the sentance below to install packages 
#hit cmd + enter to run this code or highlight it and click run in the top right corner

#install.packages(c("boot", "MASS","plyr","dplyr", "plot2", "tibble", "car", "reshape2",
#                  "epitools", "readxl", "tidyverse","arsenal")))
#install.packages(c("gt","gtExtras"))
#now we need to load all the packages into our r script, this is done with the
#function, library(). For instance, if we want to load the package "boot", we would
#run the function below. Now, highlight all the functions below in a row and run them
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(ggplot2)
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
library(gt)
library(gtExtras)
library(webshot2)
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
FWRA <- subset(FWRA, LF_ID != 23 & LF_ID != 24)

FWRA[FWRA=="Low Priority Data Gap"]<-"LPDG"
FWRA[FWRA=="Very Low"]<-"VL"
FWRA[FWRA=="Low"]<-"L"
FWRA[FWRA=="Moderate"]<-"M"
FWRA[FWRA=="High Priority Data Gap"]<-"HPDG"
FWRA[FWRA=="Very High"]<-"VH"
FWRA[FWRA=="High"]<-"H"

FWRA_NUMERIC <- FWRA

FWRA_NUMERIC[FWRA_NUMERIC=="HPDG"]<-"-1"
FWRA_NUMERIC[FWRA_NUMERIC=="LPDG"]<-"0"
FWRA_NUMERIC[FWRA_NUMERIC=="VL"]<-"1"
FWRA_NUMERIC[FWRA_NUMERIC=="L"]<-"2"
FWRA_NUMERIC[FWRA_NUMERIC=="M"]<-"3"
FWRA_NUMERIC[FWRA_NUMERIC=="H"]<-"4"
FWRA_NUMERIC[FWRA_NUMERIC=="VH"]<-"5"

FWRA_NUMERIC <- FWRA_NUMERIC %>% 
  mutate(across(Sarita_C:Artlish_F, as.numeric))


names <- FWRA %>% dplyr:: select(Sarita_C:Artlish_F)

watersheds <- print(unique(colnames(names)))
watersheds <- c("Sarita","Nahmint","Toquaht","Somass","Megin","Moyeha","Bedwell",
                "Cypre","Tranquil","Lower.Kennedy","Upper.Kennedy","Sand.River",
                "Clayoquot.River","Muriel.Creek","Tahsis.River","Leiner.River",
                "Tsowwin","Sucwoa","Canton","Conuma","Kaouk","Artlish")

for (i in watersheds) {

SARITA_NUMERIC <- FWRA_NUMERIC %>% dplyr:: select(starts_with(print(i)))
SARITA_NUMERIC <- cbind(FWRA$LF_ID,SARITA_NUMERIC)
colnames(SARITA_NUMERIC)
colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "FWRA$LF_ID")] <- "LF"
SARITA_NUMERIC <-SARITA_NUMERIC %>%
  rename(Current = 2)
SARITA_NUMERIC <-SARITA_NUMERIC %>%
  rename(Future = 3)
SARITA_NUMERIC<-subset(SARITA_NUMERIC, Current!= 0 & Current!= -1 & Future!= 0 & Future!= -1)

SAR <- SARITA_NUMERIC %>%
  select(Current, Future)

SARITA_NUMERIC <- SARITA_NUMERIC %>% rowwise() %>%
  dplyr::mutate(Multiple = Current * Future) 

SARITA_NUMERIC$Rank <- rank(-SARITA_NUMERIC$Multiple)

col_order <- c("LF","Rank","Multiple","Current", "Future")
SARITA_NUMERIC <- SARITA_NUMERIC[, col_order]
SARITA_NUMERIC <- SARITA_NUMERIC[order(SARITA_NUMERIC$Multiple, decreasing = TRUE),]  
SARITA_NUMERIC <- SARITA_NUMERIC %>% 
  mutate(across(Current:Future, as.character))

SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="1"]<-"VL"
SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="2"]<-"L"
SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="3"]<-"M"
SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="4"]<-"H"
SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="5"]<-"VH"
SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="1"]<-"VL"
SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="2"]<-"L"
SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="3"]<-"M"
SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="4"]<-"H"
SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="5"]<-"VH"

mycols <- rev(c("red3","darkorange1","gold1","yellowgreen","forestgreen"))
cols <- colorRampPalette(mycols)

SARITA_NUMERIC %>%
    head(68) %>%
    gt() %>%
gtsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_23_Tables/", print(i),".docx"))

}

#####Watershed Data Gaps######

for (i in watersheds) {
  
  SARITA_NUMERIC <- FWRA_NUMERIC %>% dplyr:: select(starts_with(print(i)))
  SARITA_NUMERIC <- cbind(FWRA$LF_ID,SARITA_NUMERIC)
  colnames(SARITA_NUMERIC)
  colnames(SARITA_NUMERIC)[which(names(SARITA_NUMERIC) == "FWRA$LF_ID")] <- "LF"
  SARITA_NUMERIC <-SARITA_NUMERIC %>%
    rename(Current = 2)
  SARITA_NUMERIC <-SARITA_NUMERIC %>%
    rename(Future = 3)
  SARITA_NUMERIC<-subset(SARITA_NUMERIC, Current!= 1 & Current!= 2 & Current!= 3 & Current!= 4 & Current!= 5 & Future!= 1 & Future!= 2 & Future!= 3 & Future!= 4 & Future!= 5)
  
  SAR <- SARITA_NUMERIC %>%
    select(Current, Future)
  
  SARITA_NUMERIC <- SARITA_NUMERIC %>% rowwise() %>%
    dplyr::mutate(Multiple = Current * Future) 
  
  SARITA_NUMERIC$Rank <- rank(-SARITA_NUMERIC$Multiple)
  
  col_order <- c("LF","Rank","Multiple","Current", "Future")
  SARITA_NUMERIC <- SARITA_NUMERIC[, col_order]
  SARITA_NUMERIC <- SARITA_NUMERIC[order(SARITA_NUMERIC$Multiple, decreasing = TRUE),]  
  SARITA_NUMERIC <- SARITA_NUMERIC %>% 
    mutate(across(Current:Future, as.character))
  
  SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="-1"]<-"HPDG"
  SARITA_NUMERIC$Current[SARITA_NUMERIC$Current=="0"]<-"LPDG"
  SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="-1"]<-"HPDG"
  SARITA_NUMERIC$Future[SARITA_NUMERIC$Future=="0"]<-"LPDG"
 
  SARITA_NUMERIC <- SARITA_NUMERIC %>%
    select(LF,Current, Future)
  
  mycols <- rev(c("red3","darkorange1","gold1","yellowgreen","forestgreen"))
  cols <- colorRampPalette(mycols)
  
  SARITA_NUMERIC %>%
    head(68) %>%
    gt() %>%
    gtsave(filename = paste0("/Users/critty/Desktop/Dekstop/GitHub/FWRA_2022_Analysis/Figures/Area_23_DG_Tables/", print(i),".docx"))
  
}


 #to view the spread sheet, type view(nuseds)
#to get a l ist og all the different varaibles or coloumn names run the code below
colnames(FWRA)


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

#Compiling Area 23
Area23_C_Sums <- Area23_C_Sums %>% 
  add_column(Time = "Current",
             .after = "n")
Area23_F_Sums <- Area23_F_Sums %>% 
  add_column(Time = "Future",
             .after = "n")

Area23_combo <- rbind(Area23_C_Sums,Area23_F_Sums)
cols <- c("Very\n Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very\n High" = "red4", "Low Priority\n Data Gap" = "grey70", "High Priority\n Data Gap" = "purple")

Area23_combo %>%
  ggplot(aes(x=value, y = n, fill = Time)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(x = "Rating", y = "Count") + 
  theme_Publication()+
  scale_fill_viridis_d(option="magma",direction = -1,begin = 0.2, end = .80)+ ggtitle("Area 23 - Count of Risk Rankings Across All LF's") 

print(Area23_combo)


Area23_combo$denominator <- c(rep(272,14))
head(Area23_combo)

Area23_combo$Proportion <- Area23_combo$n/Area23_combo$denominator
Area23_combo$Combined <- c(rep("Combined",14))

colnames(Area23_combo)[which(names(Area23_combo) == "value")] <- "Rating"



ggplot(Area23_combo, aes(x=Time, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Rating Period", y = "Percentage") +
  theme_Publication()+ 
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Proportion of Risk Ratings and Data Gaps")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Current", "Future"), labels = c("Current \n (n = 272)","Future  \n (n = 272)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"),limits = c("Very High", "High", "Moderate", "High Priority Data Gap","Low","Very Low", "Low Priority Data Gap"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)
  
  #Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
  ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure2",".jpeg"),
         device = "jpeg",
         width = 30,
         height = 30,
         units = "cm",
         dpi = 300)

#Combined Current + Future
Area23_combo<- Area23_combo %>%                               # Specify data frame
  group_by(Rating) %>%                         # Specify group indicator
  summarise_at(vars(n),              # Specify column
               list(Count = sum))

Area23_combo$Denominator <- c(rep(544,7))
Area23_combo$Proportion <- Area23_combo$Count/Area23_combo$Denominator


##########Terminal Migration Current (1-15)######
Area23_Terminal_C <- Area23_C %>% filter(row(Area23_C) >= 1 & row(Area23_C) <= 15)
VL <- c("Very Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area23_Terminal_C <- Area23_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  



cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_C)
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_C <- tidyr::pivot_longer(Area23_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_C)
Area23_Terminal_C$Rating <- factor(Area23_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))



ggplot(Area23_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Terminal Migration - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 16),breaks=seq(1,15,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure8_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area23_Terminal_CVL <- subset(Area23_Terminal_C, Rating == "Very Low")
Area23_Terminal_CL <- subset(Area23_Terminal_C, Rating == "Low")
Area23_Terminal_CM <- subset(Area23_Terminal_C, Rating == "Moderate")
Area23_Terminal_CH <- subset(Area23_Terminal_C, Rating == "High")
Area23_Terminal_CVH <- subset(Area23_Terminal_C, Rating == "Very High")
Area23_Terminal_CHPDG <- subset(Area23_Terminal_C, Rating == "High Priority Data Gap")
Area23_Terminal_CLPDG <- subset(Area23_Terminal_C, Rating == "Low Priority Data Gap")




#####Terminal Future
Area23_Terminal_F <- Area23_F %>% filter(row(Area23_F) >= 1 & row(Area23_F) <= 15)
VL <- c("Very Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area23_Terminal_F <- Area23_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_F)
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_F <- tidyr::pivot_longer(Area23_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_F)
Area23_Terminal_F$Rating <- factor(Area23_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

ggplot(Area23_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Terminal Migration - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0, 16),breaks=seq(1,15,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure8_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area23_Terminal_FVL <- subset(Area23_Terminal_F, Rating == "Very Low")
Area23_Terminal_FL <- subset(Area23_Terminal_F, Rating == "Low")
Area23_Terminal_FM <- subset(Area23_Terminal_F, Rating == "Moderate")
Area23_Terminal_FH <- subset(Area23_Terminal_F, Rating == "High")
Area23_Terminal_FVH <- subset(Area23_Terminal_F, Rating == "Very High")
Area23_Terminal_FHPDG <- subset(Area23_Terminal_F, Rating == "High Priority Data Gap")
Area23_Terminal_FLPDG <- subset(Area23_Terminal_F, Rating == "Low Priority Data Gap")

colnames(Area23_Terminal_C)

Area23_Terminal_C <- Area23_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area23_Terminal_F <- Area23_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area23_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area23_Terminal_F %>%
  mutate(prop = prop.table(Frequency))


#############Incubation (16-29)#####
Area23_Terminal_C <- Area23_C %>% filter(`FWRA$LF_ID` >= 16 & `FWRA$LF_ID` <= 29)
VL <- c("Very Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area23_Terminal_C <- Area23_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_C)
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_C <- tidyr::pivot_longer(Area23_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_C)
Area23_Terminal_C$Rating <- factor(Area23_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))


ggplot(Area23_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Incubation - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(15, 30),breaks=seq(16,29,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure9_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area23_Incubation_CVL <- subset(Area23_Terminal_C, Rating == "Very Low")
Area23_Incubation_CL <- subset(Area23_Terminal_C, Rating == "Low")
Area23_Incubation_CM <- subset(Area23_Terminal_C, Rating == "Moderate")
Area23_Incubation_CH <- subset(Area23_Terminal_C, Rating == "High")
Area23_Incubation_CVH <- subset(Area23_Terminal_C, Rating == "Very High")
Area23_Incubation_CHPDG <- subset(Area23_Terminal_C, Rating == "High Priority Data Gap")
Area23_Incubation_CLPDG <- subset(Area23_Terminal_C, Rating == "Low Priority Data Gap")


#Incubation Future
Area23_Terminal_F <- Area23_F %>% filter(`FWRA$LF_ID` >= 16 & `FWRA$LF_ID` <= 29)
VL <- c("Very Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")

Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area23_Terminal_F <- Area23_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_F)
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_F <- tidyr::pivot_longer(Area23_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_F)
Area23_Terminal_F$Rating <- factor(Area23_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

ggplot(Area23_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Incubation - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(15, 30),breaks=seq(16,29,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure9_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)



Area23_Incubation_FVL <- subset(Area23_Terminal_F, Rating == "Very Low")
Area23_Incubation_FL <- subset(Area23_Terminal_F, Rating == "Low")
Area23_Incubation_FM <- subset(Area23_Terminal_F, Rating == "Moderate")
Area23_Incubation_FH <- subset(Area23_Terminal_F, Rating == "High")
Area23_Incubation_FVH <- subset(Area23_Terminal_F, Rating == "Very High")
Area23_Incubation_FHPDG <- subset(Area23_Terminal_F, Rating == "High Priority Data Gap")
Area23_Incubation_FLPDG <- subset(Area23_Terminal_F, Rating == "Low Priority Data Gap")


Area23_Terminal_C <- Area23_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area23_Terminal_F <- Area23_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area23_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area23_Terminal_F %>%
  mutate(prop = prop.table(Frequency))

###############Freshwater Rearing (30-46)##############
###Freshwater Rearing Current
Area23_Terminal_C <- Area23_C %>% filter(`FWRA$LF_ID` >= 30 & `FWRA$LF_ID` <= 46)
VL <- c("Very Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area23_Terminal_C <- Area23_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_C)
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_C <- tidyr::pivot_longer(Area23_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_C)
Area23_Terminal_C$Rating <- factor(Area23_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))



ggplot(Area23_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Freshwater Rearing - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(29, 47),breaks=seq(30,46,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure10_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area23_EarlyRearingRiver_CVL <- subset(Area23_Terminal_C, Rating == "Very Low")
Area23_EarlyRearingRiver_CL <- subset(Area23_Terminal_C, Rating == "Low")
Area23_EarlyRearingRiver_CM <- subset(Area23_Terminal_C, Rating == "Moderate")
Area23_EarlyRearingRiver_CH <- subset(Area23_Terminal_C, Rating == "High")
Area23_EarlyRearingRiver_CVH <- subset(Area23_Terminal_C, Rating == "Very High")
Area23_EarlyRearingRiver_CHPDG <- subset(Area23_Terminal_C, Rating == "High Priority Data Gap")
Area23_EarlyRearingRiver_CLPDG <- subset(Area23_Terminal_C, Rating == "Low Priority Data Gap")


#Freshwater Rearing Future
Area23_Terminal_F <- Area23_F %>% filter(`FWRA$LF_ID` >= 30 & `FWRA$LF_ID` <= 46)
VL <- c("Very Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area23_Terminal_F <- Area23_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_F)
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_F <- tidyr::pivot_longer(Area23_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_F)
Area23_Terminal_F$Rating <- factor(Area23_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

ggplot(Area23_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Freshwater Rearing - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(29, 47),breaks=seq(30,46,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure10_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)



Area23_EarlyRearingRiver_FVL <- subset(Area23_Terminal_F, Rating == "Very Low")
Area23_EarlyRearingRiver_FL <- subset(Area23_Terminal_F, Rating == "Low")
Area23_EarlyRearingRiver_FM <- subset(Area23_Terminal_F, Rating == "Moderate")
Area23_EarlyRearingRiver_FH <- subset(Area23_Terminal_F, Rating == "High")
Area23_EarlyRearingRiver_FVH <- subset(Area23_Terminal_F, Rating == "Very High")
Area23_EarlyRearingRiver_FHPDG <- subset(Area23_Terminal_F, Rating == "High Priority Data Gap")
Area23_EarlyRearingRiver_FLPDG <- subset(Area23_Terminal_F, Rating == "Low Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area23_Terminal_F <- Area23_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area23_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area23_Terminal_F %>%
  mutate(prop = prop.table(Frequency))

################ Estuary Rearing (47-66)#########
# Estuary Rearing Current
Area23_Terminal_C <- Area23_C %>% filter(`FWRA$LF_ID` >= 47 & `FWRA$LF_ID` <= 66)
VL <- c("Very Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area23_Terminal_C <- Area23_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_C)
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_C <- tidyr::pivot_longer(Area23_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_C)
Area23_Terminal_C$Rating <- factor(Area23_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))



ggplot(Area23_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Estuary Rearing - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(46, 67),breaks=seq(47,66,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure11_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)



Area23_EarlyRearingEstuary_CVL <- subset(Area23_Terminal_C, Rating == "Very Low")
Area23_EarlyRearingEstuary_CL <- subset(Area23_Terminal_C, Rating == "Low")
Area23_EarlyRearingEstuary_CM <- subset(Area23_Terminal_C, Rating == "Moderate")
Area23_EarlyRearingEstuary_CH <- subset(Area23_Terminal_C, Rating == "High")
Area23_EarlyRearingEstuary_CVH <- subset(Area23_Terminal_C, Rating == "Very High")
Area23_EarlyRearingEstuary_CHPDG <- subset(Area23_Terminal_C, Rating == "High Priority Data Gap")
Area23_EarlyRearingEstuary_CLPDG <- subset(Area23_Terminal_C, Rating == "Low Priority Data Gap")


#Early Rearing in the Estuary Future
Area23_Terminal_F <- Area23_F %>% filter(`FWRA$LF_ID` >= 47 & `FWRA$LF_ID` <= 66)
VL <- c("Very Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area23_Terminal_F <- Area23_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_F)
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_F <- tidyr::pivot_longer(Area23_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_F)
Area23_Terminal_F$Rating <- factor(Area23_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

ggplot(Area23_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Estuary Rearing - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(46, 67),breaks=seq(47,66,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure11_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)



Area23_EarlyRearingEstuary_FVL <- subset(Area23_Terminal_F, Rating == "Very Low")
Area23_EarlyRearingEstuary_FL <- subset(Area23_Terminal_F, Rating == "Low")
Area23_EarlyRearingEstuary_FM <- subset(Area23_Terminal_F, Rating == "Moderate")
Area23_EarlyRearingEstuary_FH <- subset(Area23_Terminal_F, Rating == "High")
Area23_EarlyRearingEstuary_FVH <- subset(Area23_Terminal_F, Rating == "Very High")
Area23_EarlyRearingEstuary_FHPDG <- subset(Area23_Terminal_F, Rating == "High Priority Data Gap")
Area23_EarlyRearingEstuary_FLPDG <- subset(Area23_Terminal_F, Rating == "Low Priority Data Gap")

Area23_Terminal_C <- Area23_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area23_Terminal_F <- Area23_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area23_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area23_Terminal_F %>%
  mutate(prop = prop.table(Frequency))


##############Hatcheries and Genetics (67-70)#########
Area23_Terminal_C <- Area23_C %>% filter(`FWRA$LF_ID` >= 67 & `FWRA$LF_ID` <= 70)
VL <- c("Very Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Low = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(High = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_C <- Area23_Terminal_C %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_C")), ~sum(c(...) %in% HP)))
Area23_Terminal_C <- Area23_Terminal_C %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_C)
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_C)[which(names(Area23_Terminal_C) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_C <- tidyr::pivot_longer(Area23_Terminal_C, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_C)
Area23_Terminal_C$Rating <- factor(Area23_Terminal_C$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))



ggplot(Area23_Terminal_C, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Hatcheries & Genetics - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(66, 71),breaks=seq(67,70,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure12_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area23_HatcheriesGenetics_CVL <- subset(Area23_Terminal_C, Rating == "Very Low")
Area23_HatcheriesGenetics_CL <- subset(Area23_Terminal_C, Rating == "Low")
Area23_HatcheriesGenetics_CM <- subset(Area23_Terminal_C, Rating == "Moderate")
Area23_HatcheriesGenetics_CH <- subset(Area23_Terminal_C, Rating == "High")
Area23_HatcheriesGenetics_CVH <- subset(Area23_Terminal_C, Rating == "Very High")
Area23_HatcheriesGenetics_CHPDG <- subset(Area23_Terminal_C, Rating == "High Priority Data Gap")
Area23_HatcheriesGenetics_CLPDG <- subset(Area23_Terminal_C, Rating == "Low Priority Data Gap")


#Hatcheries & Genetics - Future Rating
Area23_Terminal_F <- Area23_F %>% filter(`FWRA$LF_ID` >= 67 & `FWRA$LF_ID` <= 70)
VL <- c("Very Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryLow = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VL)))
L <- c("Low")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Low = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% L)))
M <- c("Moderate")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(Moderate = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% M)))
H <- c("High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(High = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% H)))
VH <- c("Very High")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(VeryHigh = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% VH)))
LP <- c("Low Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(LowPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% LP)))
HP <- c("High Priority Data Gap")
Area23_Terminal_F <- Area23_Terminal_F %>% 
  mutate(HighPriorityDataGap = pmap_int(select(., ends_with("_F")), ~sum(c(...) %in% HP)))
Area23_Terminal_F <- Area23_Terminal_F %>% select(`FWRA$LF_ID`,`VeryLow`,`Low`, `Moderate`, `High`,`VeryHigh`,`LowPriorityDataGap`,`HighPriorityDataGap`)  

cols <- c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "orange", "Very High" = "red4", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "purple")

library(dplyr)
colnames(Area23_Terminal_F)
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "FWRA$LF_ID")] <- "LimitingFactor"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryHigh")] <- "Very High"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "VeryLow")] <- "Very Low"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "HighPriorityDataGap")] <- "High Priority Data Gap"
colnames(Area23_Terminal_F)[which(names(Area23_Terminal_F) == "LowPriorityDataGap")] <- "Low Priority Data Gap"


require(reshape2)
Area23_Terminal_F <- tidyr::pivot_longer(Area23_Terminal_F, cols=c("Very Low","Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"), names_to='Rating', 
                                         values_to="Proportion")
colnames(Area23_Terminal_F)
Area23_Terminal_F$Rating <- factor(Area23_Terminal_F$Rating, c("High Priority Data Gap", "Low Priority Data Gap","Very High","High","Moderate","Low","Very Low"))

ggplot(Area23_Terminal_F, aes(x=LimitingFactor, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Hatcheries & Genetics - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(66, 71),breaks=seq(67,70,1))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,4), breaks=seq(0,4,1))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure12_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


Area23_HatcheriesGenetics_FVL <- subset(Area23_Terminal_F, Rating == "Very Low")
Area23_HatcheriesGenetics_FL <- subset(Area23_Terminal_F, Rating == "Low")
Area23_HatcheriesGenetics_FM <- subset(Area23_Terminal_F, Rating == "Moderate")
Area23_HatcheriesGenetics_FH <- subset(Area23_Terminal_F, Rating == "High")
Area23_HatcheriesGenetics_FVH <- subset(Area23_Terminal_F, Rating == "Very High")
Area23_HatcheriesGenetics_FHPDG <- subset(Area23_Terminal_F, Rating == "High Priority Data Gap")
Area23_HatcheriesGenetics_FLPDG <- subset(Area23_Terminal_F, Rating == "Low Priority Data Gap")

Area23_Terminal_C <- Area23_Terminal_C %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

Area23_Terminal_F <- Area23_Terminal_F %>% 
  group_by(Rating) %>% 
  summarise(Frequency = sum(Proportion))

library(dplyr)

Area23_Terminal_C %>%
  mutate(prop = prop.table(Frequency))
Area23_Terminal_F %>%
  mutate(prop = prop.table(Frequency))


##########Ratings Current Stacked Bar########
VeryLow_All_C <- rbind(Area23_Terminal_CVL,Area23_Incubation_CVL,Area23_EarlyRearingRiver_CVL,Area23_EarlyRearingEstuary_CVL,Area23_HatcheriesGenetics_CVL)
Low_All_C <- rbind(Area23_Terminal_CL,Area23_Incubation_CL,Area23_EarlyRearingRiver_CL,Area23_EarlyRearingEstuary_CL,Area23_HatcheriesGenetics_CL)
Moderate_All_C <- rbind(Area23_Terminal_CM,Area23_Incubation_CM,Area23_EarlyRearingRiver_CM,Area23_EarlyRearingEstuary_CM,Area23_HatcheriesGenetics_CM)
High_All_C <- rbind(Area23_Terminal_CH,Area23_Incubation_CH,Area23_EarlyRearingRiver_CH,Area23_EarlyRearingEstuary_CH,Area23_HatcheriesGenetics_CH)
VeryHigh_All_C <- rbind(Area23_Terminal_CVH,Area23_Incubation_CVH,Area23_EarlyRearingRiver_CVH,Area23_EarlyRearingEstuary_CVH,Area23_HatcheriesGenetics_CVH)
Upper_C<-rbind(VeryLow_All_C,Low_All_C,Moderate_All_C,High_All_C,VeryHigh_All_C)
library(dplyr)

Upper_C<- Upper_C %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

Upper_C_Final <- Upper_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

Upper_C_Sums <- Upper_C_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))

Upper_C_Final$Denominator <- c(20,30,14,14,32,20,30,14,14,32,20,30,14,14,32,20,30,14,14,32,20,30,14,14,32)
Upper_C_Final$Proportion <- Upper_C_Final$newcount/Upper_C_Final$Denominator

ggplot(Upper_C_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 32)","Incubation\n (n = 14)","Freshwater\nRearing \n (n = 30)","Estuary\nRearing\n (n = 20)","Hatcheries &\n Genetics\n (n = 14)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,40), breaks=seq(0,40,10))



##########Ratings Future Stacked Bar########
VeryLow_All_F <- rbind(Area23_Terminal_FVL,Area23_Incubation_FVL,Area23_EarlyRearingRiver_FVL,Area23_EarlyRearingEstuary_FVL,Area23_HatcheriesGenetics_FVL)
Low_All_F <- rbind(Area23_Terminal_FL,Area23_Incubation_FL,Area23_EarlyRearingRiver_FL,Area23_EarlyRearingEstuary_FL,Area23_HatcheriesGenetics_FL)
Moderate_All_F <- rbind(Area23_Terminal_FM,Area23_Incubation_FM,Area23_EarlyRearingRiver_FM,Area23_EarlyRearingEstuary_FM,Area23_HatcheriesGenetics_FM)
High_All_F <- rbind(Area23_Terminal_FH,Area23_Incubation_FH,Area23_EarlyRearingRiver_FH,Area23_EarlyRearingEstuary_FH,Area23_HatcheriesGenetics_FH)
VeryHigh_All_F <- rbind(Area23_Terminal_FVH,Area23_Incubation_FVH,Area23_EarlyRearingRiver_FVH,Area23_EarlyRearingEstuary_FVH,Area23_HatcheriesGenetics_FVH)


Upper_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F)
library(dplyr)

Upper_F<- Upper_F %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

Upper_F_Sums<- Upper_F_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))

Upper_F_Final$Denominator <- c(20,30,14,14,32,20,30,14,14,32,20,30,14,14,32,20,30,14,14,32,20,30,14,14,32)

Upper_F_Final$Proportion <- Upper_F_Final$newcount/Upper_F_Final$Denominator
ggplot(Upper_F_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings Grouped by Lifestage - Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 32)","Incubation\n (n = 14)","Freshwater\nRearing \n (n = 30)","Estuary\nRearing\n (n = 20)","Hatcheries &\n Genetics\n (n = 14)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,40), breaks=seq(0,40,10))
"53,62,12,50,90"


##########All Current Stacked Bar########
VeryLow_All_C <- rbind(Area23_Terminal_CVL,Area23_Incubation_CVL,Area23_EarlyRearingRiver_CVL,Area23_EarlyRearingEstuary_CVL,Area23_HatcheriesGenetics_CVL)
Low_All_C <- rbind(Area23_Terminal_CL,Area23_Incubation_CL,Area23_EarlyRearingRiver_CL,Area23_EarlyRearingEstuary_CL,Area23_HatcheriesGenetics_CL)
Moderate_All_C <- rbind(Area23_Terminal_CM,Area23_Incubation_CM,Area23_EarlyRearingRiver_CM,Area23_EarlyRearingEstuary_CM,Area23_HatcheriesGenetics_CM)
High_All_C <- rbind(Area23_Terminal_CH,Area23_Incubation_CH,Area23_EarlyRearingRiver_CH,Area23_EarlyRearingEstuary_CH,Area23_HatcheriesGenetics_CH)
VeryHigh_All_C <- rbind(Area23_Terminal_CVH,Area23_Incubation_CVH,Area23_EarlyRearingRiver_CVH,Area23_EarlyRearingEstuary_CVH,Area23_HatcheriesGenetics_CVH)
Low_DG_C <- rbind(Area23_Terminal_CLPDG,Area23_Incubation_CLPDG,Area23_EarlyRearingRiver_CLPDG,Area23_EarlyRearingEstuary_CLPDG,Area23_HatcheriesGenetics_CLPDG)
High_DG_C <- rbind(Area23_Terminal_CHPDG,Area23_Incubation_CHPDG,Area23_EarlyRearingRiver_CHPDG,Area23_EarlyRearingEstuary_CHPDG,Area23_HatcheriesGenetics_CHPDG)
ALL_C<-rbind(VeryLow_All_C,Low_All_C,Moderate_All_C,High_All_C,VeryHigh_All_C,Low_DG_C,High_DG_C)

library(dplyr)

ALL_C<- ALL_C %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)


ALL_C <- ALL_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_C_Sums<- ALL_C %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_C$Denominator <- c(80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60)

ALL_C$Proportion <- ALL_C$newcount/ALL_C$Denominator
ggplot(ALL_C, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings & Data Gaps Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 80)","Incubation\n (n = 68)","Freshwater\nRearing \n (n = 16)","Estuary\nRearing\n (n = 48)","Hatcheries &\n Genetics\n (n = 60)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,25))


#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure3_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

##########All Future Stacked Bar########
VeryLow_All_F <- rbind(Area23_Terminal_FVL,Area23_Incubation_FVL,Area23_EarlyRearingRiver_FVL,Area23_EarlyRearingEstuary_FVL,Area23_HatcheriesGenetics_FVL)
Low_All_F <- rbind(Area23_Terminal_FL,Area23_Incubation_FL,Area23_EarlyRearingRiver_FL,Area23_EarlyRearingEstuary_FL,Area23_HatcheriesGenetics_FL)
Moderate_All_F <- rbind(Area23_Terminal_FM,Area23_Incubation_FM,Area23_EarlyRearingRiver_FM,Area23_EarlyRearingEstuary_FM,Area23_HatcheriesGenetics_FM)
High_All_F <- rbind(Area23_Terminal_FH,Area23_Incubation_FH,Area23_EarlyRearingRiver_FH,Area23_EarlyRearingEstuary_FH,Area23_HatcheriesGenetics_FH)
VeryHigh_All_F <- rbind(Area23_Terminal_FVH,Area23_Incubation_FVH,Area23_EarlyRearingRiver_FVH,Area23_EarlyRearingEstuary_FVH,Area23_HatcheriesGenetics_FVH)
Low_DG_F <- rbind(Area23_Terminal_FLPDG,Area23_Incubation_FLPDG,Area23_EarlyRearingRiver_FLPDG,Area23_EarlyRearingEstuary_FLPDG,Area23_HatcheriesGenetics_FLPDG)
High_DG_F <- rbind(Area23_Terminal_FHPDG,Area23_Incubation_FHPDG,Area23_EarlyRearingRiver_FHPDG,Area23_EarlyRearingEstuary_FHPDG,Area23_HatcheriesGenetics_FHPDG)

ALL_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F,Low_DG_F,High_DG_F)
library(dplyr)

ALL_F<- ALL_F %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

ALL_F <- ALL_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_F_Sums<- ALL_F %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_F$Denominator <- c(80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60)

ALL_F$Proportion <- ALL_F$newcount/ALL_F$Denominator
ggplot(ALL_F, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings & Data Gaps Grouped by Lifestage - Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 80)","Incubation\n (n = 68)","Freshwater\nRearing \n (n = 16)","Estuary\nRearing\n (n = 48)","Hatcheries &\n Genetics\n (n = 60)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure3_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

##########Upper Current Stacked Bar########
Moderate_All_C <- rbind(Area24_Terminal_CM,Area24_Incubation_CM,Area23_EarlyRearingRiver_CM,Area23_EarlyRearingEstuary_CM,Area23_HatcheriesGenetics_CM)
High_All_C <- rbind(Area23_Terminal_CH,Area23_Incubation_CH,Area23_EarlyRearingRiver_CH,Area23_EarlyRearingEstuary_CH,Area23_HatcheriesGenetics_CH)
VeryHigh_All_C <- rbind(Area23_Terminal_CVH,Area23_Incubation_CVH,Area23_EarlyRearingRiver_CVH,Area23_EarlyRearingEstuary_CVH,Area23_HatcheriesGenetics_CVH)


Upper_C<-rbind(Moderate_All_C,High_All_C,VeryHigh_All_C)
library(dplyr)

Upper_C<- Upper_C %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

Upper_C_Final <- Upper_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 


Upper_C_Sums<- Upper_C_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_C_Final$Denominator <- c(80,68,16,48,60,80,68,16,48,60,80,68,16,48,60)
Upper_C_Final$Proportion <- Upper_C_Final$newcount/Upper_C_Final$Denominator

ggplot(Upper_C_Final, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Count") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - High Risk LF's Grouped by Lifestage - Current Rating")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 80)","Incubation\n (n = 68)","Freshwater\nRearing \n (n = 16)","Estuary\nRearing\n (n = 48)","Hatcheries &\n Genetics\n (n = 60)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,20), breaks=seq(0,20,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure5_C",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

##########Upper Future Stacked Bar########
Moderate_All_F <- rbind(Area23_Terminal_FM,Area23_Incubation_FM,Area23_EarlyRearingRiver_FM,Area23_EarlyRearingEstuary_FM,Area23_HatcheriesGenetics_FM)
High_All_F <- rbind(Area23_Terminal_FH,Area23_Incubation_FH,Area23_EarlyRearingRiver_FH,Area23_EarlyRearingEstuary_FH,Area23_HatcheriesGenetics_FH)
VeryHigh_All_F <- rbind(Area23_Terminal_FVH,Area23_Incubation_FVH,Area23_EarlyRearingRiver_FVH,Area23_EarlyRearingEstuary_FVH,Area23_HatcheriesGenetics_FVH)
Upper_F<-rbind(Moderate_All_F,High_All_F,VeryHigh_All_F)
library(dplyr)

Upper_F<- Upper_F %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
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
  ggtitle("Area 23 - High Risk LF's Grouped by Lifestage - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 80)","Incubation\n (n = 68)","Freshwater\nRearing \n (n = 16)","Estuary\nRearing\n (n = 48)","Hatcheries &\n Genetics\n (n = 60)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,20), breaks=seq(0,20,5))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure5_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

##########DG Current & Future Stacked Bar########
Low_DG_C <- rbind(Area23_Terminal_CLPDG,Area23_Incubation_CLPDG,Area23_EarlyRearingRiver_CLPDG,Area23_EarlyRearingEstuary_CLPDG,Area23_HatcheriesGenetics_CLPDG)
High_DG_C <- rbind(Area23_Terminal_CHPDG,Area23_Incubation_CHPDG,Area23_EarlyRearingRiver_CHPDG,Area23_EarlyRearingEstuary_CHPDG,Area23_HatcheriesGenetics_CHPDG)
DG_C<-rbind(Low_DG_C,High_DG_C)

Low_DG_F <- rbind(Area23_Terminal_FLPDG,Area23_Incubation_FLPDG,Area23_EarlyRearingRiver_FLPDG,Area23_EarlyRearingEstuary_FLPDG,Area23_HatcheriesGenetics_FLPDG)
High_DG_F <- rbind(Area23_Terminal_FHPDG,Area23_Incubation_FHPDG,Area23_EarlyRearingRiver_FHPDG,Area23_EarlyRearingEstuary_FHPDG,Area23_HatcheriesGenetics_FHPDG)
DG_F<-rbind(Low_DG_F,High_DG_F)

DG_CF<-rbind(DG_C,DG_F)

library(dplyr)

library(dplyr)

DG_CF<- DG_CF %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)
DG_CF <- DG_CF %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

DG_CF_Sums<- DG_CF %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


DG_CF$Denominator <- c(120,76,4,68,56,120,76,4,68,56)
DG_CF$Proportion <- DG_CF$newcount/DG_CF$Denominator


ggplot(DG_CF, aes(x=Group, y=newcount, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Count of Data Gaps") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Data Gaps Grouped by Lifestage - Current & Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 120)","Incubation\n (n = 140)","Freshwater\nRearing\n (n = 216)","Estuary\nRearing\n (n = 294)","Hatcheries &\n Genetics\n (n = 56)"))+
  scale_fill_manual(values = c("Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,125), breaks=seq(0,125,25))

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure6",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

###########Proportional Life Stage Breakdown - Future All############
VeryLow_All_F <- rbind(Area23_Terminal_FVL,Area23_Incubation_FVL,Area23_EarlyRearingRiver_FVL,Area23_EarlyRearingEstuary_FVL,Area23_HatcheriesGenetics_FVL)
Low_All_F <- rbind(Area23_Terminal_FL,Area23_Incubation_FL,Area23_EarlyRearingRiver_FL,Area23_EarlyRearingEstuary_FL,Area23_HatcheriesGenetics_FL)
Moderate_All_F <- rbind(Area23_Terminal_FM,Area23_Incubation_FM,Area23_EarlyRearingRiver_FM,Area23_EarlyRearingEstuary_FM,Area23_HatcheriesGenetics_FM)
High_All_F <- rbind(Area23_Terminal_FH,Area23_Incubation_FH,Area23_EarlyRearingRiver_FH,Area23_EarlyRearingEstuary_FH,Area23_HatcheriesGenetics_FH)
VeryHigh_All_F <- rbind(Area23_Terminal_FVH,Area23_Incubation_FVH,Area23_EarlyRearingRiver_FVH,Area23_EarlyRearingEstuary_FVH,Area23_HatcheriesGenetics_FVH)


Upper_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F)
library(dplyr)

Upper_F<- Upper_F %>% mutate(Group =
                               case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                         between(LimitingFactor,16,29) ~ "Incubation",
                                         between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                         between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                         between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

Upper_F_Final <- Upper_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 


Upper_F_Sums<- Upper_F_Final %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


Upper_F_Final$Denominator <- c(20,30,14,14,32,20,30,14,14,32,20,30,14,14,32,20,30,14,14,32,20,30,14,14,32)
Upper_F_Final$Proportion <- Upper_F_Final$newcount/Upper_F_Final$Denominator

ggplot(Upper_F_Final, aes(x=Group, y=Proportion, fill = Rating, label = newcount))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - LF's Grouped by Lifestage - Future Rating")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 32)","Incubation\n (n = 14)","Freshwater\nRearing \n (n = 30)","Estuary\nRearing\n (n = 20)","Hatcheries &\n Genetics\n (n = 14)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)


######Proportional Data Gap Life Stage Breakdown - Current & Future All############
Low_DG_C <- rbind(Area23_Terminal_CLPDG,Area23_Incubation_CLPDG,Area23_EarlyRearingRiver_CLPDG,Area23_EarlyRearingEstuary_CLPDG,Area23_HatcheriesGenetics_CLPDG)
High_DG_C <- rbind(Area23_Terminal_CHPDG,Area23_Incubation_CHPDG,Area23_EarlyRearingRiver_CHPDG,Area23_EarlyRearingEstuary_CHPDG,Area23_HatcheriesGenetics_CHPDG)
Low_DG_F <- rbind(Area23_Terminal_FLPDG,Area23_Incubation_FLPDG,Area23_EarlyRearingRiver_FLPDG,Area23_EarlyRearingEstuary_FLPDG,Area23_HatcheriesGenetics_FLPDG)
High_DG_F <- rbind(Area23_Terminal_FHPDG,Area23_Incubation_FHPDG,Area23_EarlyRearingRiver_FHPDG,Area23_EarlyRearingEstuary_FHPDG,Area23_HatcheriesGenetics_FHPDG)
DG_F<-rbind(Low_DG_F,High_DG_F)
DG_C<-rbind(Low_DG_C,High_DG_C)

DG_CF<-rbind(DG_C,DG_F)

DG_CF<- DG_CF %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)
DG_CF <- DG_CF %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

DG_CF_Sums<- DG_CF %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


DG_CF$Denominator <- c(120,76,4,68,56,120,76,4,68,56)
DG_CF$Proportion <- DG_CF$newcount/DG_CF$Denominator


ggplot(DG_CF, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Life Stage", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Data Gaps Grouped by Lifestage - Current & Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (56)","Incubation\n (n = 68)","Freshwater\nRearing\n (n = 76)","Estuary\nRearing\n (n = 120)","Hatcheries &\n Genetics\n (n = 4)"))+
  scale_fill_manual(values = c("Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure7",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)

##########All Current Proprtion Bar########
VeryLow_All_C <- rbind(Area23_Terminal_CVL,Area23_Incubation_CVL,Area23_EarlyRearingRiver_CVL,Area23_EarlyRearingEstuary_CVL,Area23_HatcheriesGenetics_CVL)
Low_All_C <- rbind(Area23_Terminal_CL,Area23_Incubation_CL,Area23_EarlyRearingRiver_CL,Area23_EarlyRearingEstuary_CL,Area23_HatcheriesGenetics_CL)
Moderate_All_C <- rbind(Area23_Terminal_CM,Area23_Incubation_CM,Area23_EarlyRearingRiver_CM,Area23_EarlyRearingEstuary_CM,Area23_HatcheriesGenetics_CM)
High_All_C <- rbind(Area23_Terminal_CH,Area23_Incubation_CH,Area23_EarlyRearingRiver_CH,Area23_EarlyRearingEstuary_CH,Area23_HatcheriesGenetics_CH)
VeryHigh_All_C <- rbind(Area23_Terminal_CVH,Area23_Incubation_CVH,Area23_EarlyRearingRiver_CVH,Area23_EarlyRearingEstuary_CVH,Area23_HatcheriesGenetics_CVH)
Low_DG_C <- rbind(Area23_Terminal_CLPDG,Area23_Incubation_CLPDG,Area23_EarlyRearingRiver_CLPDG,Area23_EarlyRearingEstuary_CLPDG,Area23_HatcheriesGenetics_CLPDG)
High_DG_C <- rbind(Area23_Terminal_CHPDG,Area23_Incubation_CHPDG,Area23_EarlyRearingRiver_CHPDG,Area23_EarlyRearingEstuary_CHPDG,Area23_HatcheriesGenetics_CHPDG)

ALL_C<-rbind(VeryLow_All_C,Low_All_C,Moderate_All_C,High_All_C,VeryHigh_All_C,Low_DG_C,High_DG_C)
library(dplyr)

ALL_C<- ALL_C %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

ALL_C <- ALL_C %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_C_Sums<- ALL_C %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_C$Denominator <- c(80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60)

ALL_C$Proportion <- ALL_C$newcount/ALL_C$Denominator
ggplot(ALL_C, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings & Data Gaps Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 60)","Incubation\n (n = 48)","Freshwater\nRearing \n (n = 68)","Estuary\nRearing\n (n = 80)","Hatcheries &\n Genetics\n (n = 16)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure4_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)
##########All Future Stacked Bar########
VeryLow_All_F <- rbind(Area23_Terminal_FVL,Area23_Incubation_FVL,Area23_EarlyRearingRiver_FVL,Area23_EarlyRearingEstuary_FVL,Area23_HatcheriesGenetics_FVL)
Low_All_F <- rbind(Area23_Terminal_FL,Area23_Incubation_FL,Area23_EarlyRearingRiver_FL,Area23_EarlyRearingEstuary_FL,Area23_HatcheriesGenetics_FL)
Moderate_All_F <- rbind(Area23_Terminal_FM,Area23_Incubation_FM,Area23_EarlyRearingRiver_FM,Area23_EarlyRearingEstuary_FM,Area23_HatcheriesGenetics_FM)
High_All_F <- rbind(Area23_Terminal_FH,Area23_Incubation_FH,Area23_EarlyRearingRiver_FH,Area23_EarlyRearingEstuary_FH,Area23_HatcheriesGenetics_FH)
VeryHigh_All_F <- rbind(Area23_Terminal_FVH,Area23_Incubation_FVH,Area23_EarlyRearingRiver_FVH,Area23_EarlyRearingEstuary_FVH,Area23_HatcheriesGenetics_FVH)
Low_DG_F <- rbind(Area23_Terminal_FLPDG,Area23_Incubation_FLPDG,Area23_EarlyRearingRiver_FLPDG,Area23_EarlyRearingEstuary_FLPDG,Area23_HatcheriesGenetics_FLPDG)
High_DG_F <- rbind(Area23_Terminal_FHPDG,Area23_Incubation_FHPDG,Area23_EarlyRearingRiver_FHPDG,Area23_EarlyRearingEstuary_FHPDG,Area23_HatcheriesGenetics_FHPDG)

ALL_F<-rbind(VeryLow_All_F,Low_All_F,Moderate_All_F,High_All_F,VeryHigh_All_F,Low_DG_F,High_DG_F)
library(dplyr)

ALL_F<- ALL_F %>% mutate(Group =
                           case_when(between(LimitingFactor,1,15) ~ "Terminal", 
                                     between(LimitingFactor,16,29) ~ "Incubation",
                                     between(LimitingFactor,30,46) ~ "Freshwater Rearing",
                                     between(LimitingFactor,47,66) ~ "Estuary Rearing", 
                                     between(LimitingFactor,66,70) ~ "Hatcheries & Genetics"))
library(dplyr)

ALL_F <- ALL_F %>% group_by(Rating,Group) %>% summarise(newcount=sum(Proportion)) 

ALL_F_Sums<- ALL_F %>%                               # Specify data frame
  group_by(Group) %>%                         # Specify group indicator
  summarise_at(vars(newcount),              # Specify column
               list(Denominator = sum))


ALL_F$Denominator <- c(80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60,80,68,16,48,60)

ALL_F$Proportion <- ALL_F$newcount/ALL_F$Denominator
ggplot(ALL_F, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings & Data Gaps Grouped by Lifestage - Future")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 150)","Incubation\n (n = 120)","Freshwater\nRearing \n (n = 170)","Estuary\nRearing\n (n = 200)","Hatcheries &\n Genetics\n (n = 40)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

#Save the plot, define your folder location as "/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures"
ggsave(filename = paste0("/Users/user/Documents/GitHub/FWRA_2022_Analysis/Figures/Area_23_Figures/", "Figure4_F",".jpeg"),
       device = "jpeg",
       width = 30,
       height = 30,
       units = "cm",
       dpi = 300)


unique(Area23$Nahmint_C)

#######Sorted LF's by watershed
#Area 23 Sarita
Sarita <- Area23 %>% select(starts_with("Sarita")) %>%
  gather(key, value) %>%
  count(value)

Sarita$value <- factor(Sarita$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))
colnames(Sarita)[which(names(Sarita) == "value")] <- "Sarita_Value"
colnames(Sarita)[which(names(Sarita) == "n")] <- "Sarita_Count"

#Area 23 Toquaht
Toquaht <- Area23 %>% select(starts_with("Toquaht")) %>%
  gather(key, value) %>%
  count(value)

Toquaht$value <- factor(Toquaht$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))
colnames(Toquaht)[which(names(Toquaht) == "value")] <- "Toquaht_Value"
colnames(Toquaht)[which(names(Toquaht) == "n")] <- "Toquaht_Count"

#Area 23 Nahmint
Nahmint <- Area23 %>% select(starts_with("Nahmint")) %>%
  gather(key, value) %>%
  count(value)

Nahmint$value <- factor(Nahmint$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))
colnames(Nahmint)[which(names(Nahmint) == "value")] <- "Nahmint_Value"
colnames(Nahmint)[which(names(Nahmint) == "n")] <- "Nahmint_Count"

#Area 23 Somass
Somass <- Area23 %>% select(starts_with("Somass")) %>%
  gather(key, value) %>%
  count(value)

Somass$value <- factor(Somass$value, c("Very Low", "Low", "Moderate","High","Very High","Low Priority Data Gap","High Priority Data Gap"))
colnames(Somass)[which(names(Somass) == "value")] <- "Somass_Value"
colnames(Somass)[which(names(Somass) == "n")] <- "Somass_Count"

Area23.o <- cbind(Sarita, Toquaht, Nahmint,Somass)


view(Area23.o)



####
ggplot(Area23, aes(x=Group, y=Proportion, fill = Rating))+
  geom_bar(position="stack", stat="identity")+
  labs(x = "Limiting Factor", y = "Percentage") +
  theme_Publication()+ 
  scale_fill_viridis_d(begin = 0 , end = .94, direction = 1)+
  theme(axis.text=element_text(size=14),
        axis.text.x=element_text(angle = 45, vjust = 0.8, hjust = .9, color = "black"),
        axis.text.y=element_text(color="black"))+
  ggtitle("Area 23 - Ratings & Data Gaps Grouped by Lifestage - Current")  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits = c("Terminal","Incubation","Freshwater Rearing","Estuary Rearing","Hatcheries & Genetics"), labels = c("Terminal\n Migration\n (n = 60)","Incubation\n (n = 48)","Freshwater\nRearing \n (n = 68)","Estuary\nRearing\n (n = 80)","Hatcheries &\n Genetics\n (n = 16)"))+
  scale_fill_manual(values = c("Very Low" = "forestgreen", "Low" = "yellowgreen", "Moderate"= "gold1","High" = "darkorange1", "Very High" = "red3", "Low Priority Data Gap" = "grey70", "High Priority Data Gap" = "grey30"))+
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.25), labels = scales::percent)

